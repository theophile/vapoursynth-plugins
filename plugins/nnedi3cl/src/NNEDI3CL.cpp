/*
**   VapourSynth port utilizing OpenCL by HolyWu
**
**                    nnedi3 v0.9.4 for Avisynth 2.5.x
**
**   Copyright (C) 2010-2011 Kevin Stone
**
**   This program is free software; you can redistribute it and/or modify
**   it under the terms of the GNU General Public License as published by
**   the Free Software Foundation; either version 2 of the License, or
**   (at your option) any later version.
**
**   This program is distributed in the hope that it will be useful,
**   but WITHOUT ANY WARRANTY; without even the implied warranty of
**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**   GNU General Public License for more details.
**
**   You should have received a copy of the GNU General Public License
**   along with this program; if not, write to the Free Software
**   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <cerrno>
#include <cstdio>

#include <locale>
#include <memory>
#include <sstream>
#include <string>

#include <VapourSynth.h>
#include <VSHelper.h>

#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif

#define BOOST_COMPUTE_DEBUG_KERNEL_COMPILATION
#define BOOST_COMPUTE_HAVE_THREAD_LOCAL
#define BOOST_COMPUTE_THREAD_SAFE
#define BOOST_COMPUTE_USE_OFFLINE_CACHE
#include <boost/compute/core.hpp>
#include <boost/compute/utility/dim.hpp>
namespace compute = boost::compute;

#include "NNEDI3CL.cl"

static constexpr int numNSIZE = 7;
static constexpr int numNNS = 5;
static constexpr int xdiaTable[numNSIZE] = { 8, 16, 32, 48, 8, 16, 32 };
static constexpr int ydiaTable[numNSIZE] = { 6, 6, 6, 6, 4, 4, 4 };
static constexpr int nnsTable[numNNS] = { 16, 32, 64, 128, 256 };

struct NNEDI3CLData {
    VSNodeRef * node;
    VSVideoInfo vi;
    int field;
    bool dh, dw, process[3];
    compute::command_queue queue;
    compute::kernel kernel;
    compute::image2d src, dst, tmp;
    compute::buffer weights0, weights1Buffer;
    cl_mem weights1;
};

static inline int roundds(const double f) noexcept {
    return (f - std::floor(f) >= 0.5) ? std::min(static_cast<int>(std::ceil(f)), 32767) : std::max(static_cast<int>(std::floor(f)), -32768);
}

template<typename T>
static void filter(const VSFrameRef * src, VSFrameRef * dst, const int field_n, const NNEDI3CLData * const VS_RESTRICT d, const VSAPI * vsapi) {
    for (int plane = 0; plane < d->vi.format->numPlanes; plane++) {
        if (d->process[plane]) {
            const int srcWidth = vsapi->getFrameWidth(src, plane);
            const int dstWidth = vsapi->getFrameWidth(dst, plane);
            const int srcHeight = vsapi->getFrameHeight(src, plane);
            const int dstHeight = vsapi->getFrameHeight(dst, plane);
            const T * srcp = reinterpret_cast<const T *>(vsapi->getReadPtr(src, plane));
            T * VS_RESTRICT dstp = reinterpret_cast<T *>(vsapi->getWritePtr(dst, plane));

            auto queue = d->queue;
            auto kernel = d->kernel;
            auto srcImage = d->src;
            auto dstImage = d->dst;
            auto tmpImage = d->tmp;

            constexpr size_t localWorkSize[] = { 4, 16 };

            queue.enqueue_write_image(srcImage, compute::dim(0, 0), compute::dim(srcWidth, srcHeight), srcp, vsapi->getStride(src, plane));

            if (d->dh && d->dw) {
                size_t globalWorkSize[] = { static_cast<size_t>(((srcHeight + 7) / 8 + 3) & -4), static_cast<size_t>((dstWidth / 2 + 15) & -16) };
                kernel.set_args(srcImage, tmpImage, d->weights0, d->weights1, srcHeight, srcWidth, srcHeight, dstWidth, field_n, 1 - field_n, -1);
                queue.enqueue_nd_range_kernel(kernel, 2, nullptr, globalWorkSize, localWorkSize);

                globalWorkSize[0] = static_cast<size_t>(((dstWidth + 7) / 8 + 3) & -4);
                globalWorkSize[1] = static_cast<size_t>((dstHeight / 2 + 15) & -16);
                kernel.set_args(tmpImage, dstImage, d->weights0, d->weights1, dstWidth, srcHeight, dstWidth, dstHeight, field_n, 1 - field_n, 0);
                queue.enqueue_nd_range_kernel(kernel, 2, nullptr, globalWorkSize, localWorkSize);
            } else if (d->dw) {
                const size_t globalWorkSize[] = { static_cast<size_t>(((dstHeight + 7) / 8 + 3) & -4), static_cast<size_t>((dstWidth / 2 + 15) & -16) };
                kernel.set_args(srcImage, dstImage, d->weights0, d->weights1, srcHeight, srcWidth, dstHeight, dstWidth, field_n, 1 - field_n, -1);
                queue.enqueue_nd_range_kernel(kernel, 2, nullptr, globalWorkSize, localWorkSize);
            } else {
                const size_t globalWorkSize[] = { static_cast<size_t>(((dstWidth + 7) / 8 + 3) & -4), static_cast<size_t>((dstHeight / 2 + 15) & -16) };
                kernel.set_args(srcImage, dstImage, d->weights0, d->weights1, srcWidth, srcHeight, dstWidth, dstHeight, field_n, 1 - field_n, 0);
                queue.enqueue_nd_range_kernel(kernel, 2, nullptr, globalWorkSize, localWorkSize);
            }

            queue.enqueue_read_image(dstImage, compute::dim(0, 0), compute::dim(dstWidth, dstHeight), dstp, vsapi->getStride(dst, plane));
        }
    }
}

static void VS_CC nnedi3clInit(VSMap * in, VSMap * out, void ** instanceData, VSNode * node, VSCore * core, const VSAPI * vsapi) {
    NNEDI3CLData * d = static_cast<NNEDI3CLData *>(*instanceData);
    vsapi->setVideoInfo(&d->vi, 1, node);
}

static const VSFrameRef * VS_CC nnedi3clGetFrame(int n, int activationReason, void ** instanceData, void ** frameData, VSFrameContext * frameCtx, VSCore * core, const VSAPI * vsapi) {
    NNEDI3CLData * d = static_cast<NNEDI3CLData *>(*instanceData);

    if (activationReason == arInitial) {
        vsapi->requestFrameFilter(d->field > 1 ? n / 2 : n, d->node, frameCtx);
    } else if (activationReason == arAllFramesReady) {
        const VSFrameRef * src = vsapi->getFrameFilter(d->field > 1 ? n / 2 : n, d->node, frameCtx);
        VSFrameRef * dst = vsapi->newVideoFrame(d->vi.format, d->vi.width, d->vi.height, src, core);

        int field = d->field;
        if (field > 1)
            field -= 2;

        int err;
        const int fieldBased = int64ToIntS(vsapi->propGetInt(vsapi->getFramePropsRO(src), "_FieldBased", 0, &err));
        if (fieldBased == 1)
            field = 0;
        else if (fieldBased == 2)
            field = 1;

        int field_n;
        if (d->field > 1) {
            if (n & 1)
                field_n = (field == 0);
            else
                field_n = (field == 1);
        } else {
            field_n = field;
        }

        try {
            if (d->vi.format->bytesPerSample == 1)
                filter<uint8_t>(src, dst, field_n, d, vsapi);
            else if (d->vi.format->bytesPerSample == 2)
                filter<uint16_t>(src, dst, field_n, d, vsapi);
            else
                filter<float>(src, dst, field_n, d, vsapi);
        } catch (const compute::opencl_error & error) {
            vsapi->setFilterError(("NNEDI3CL: " + error.error_string()).c_str(), frameCtx);
            vsapi->freeFrame(src);
            vsapi->freeFrame(dst);
            return nullptr;
        }

        VSMap * props = vsapi->getFramePropsRW(dst);
        vsapi->propSetInt(props, "_FieldBased", 0, paReplace);

        if (d->field > 1) {
            int errNum, errDen;
            int64_t durationNum = vsapi->propGetInt(props, "_DurationNum", 0, &errNum);
            int64_t durationDen = vsapi->propGetInt(props, "_DurationDen", 0, &errDen);
            if (!errNum && !errDen) {
                muldivRational(&durationNum, &durationDen, 1, 2);
                vsapi->propSetInt(props, "_DurationNum", durationNum, paReplace);
                vsapi->propSetInt(props, "_DurationDen", durationDen, paReplace);
            }
        }

        vsapi->freeFrame(src);
        return dst;
    }

    return nullptr;
}

static void VS_CC nnedi3clFree(void * instanceData, VSCore * core, const VSAPI * vsapi) {
    NNEDI3CLData * d = static_cast<NNEDI3CLData *>(instanceData);

    vsapi->freeNode(d->node);

    clReleaseMemObject(d->weights1);

    delete d;
}

void VS_CC nnedi3clCreate(const VSMap * in, VSMap * out, void * userData, VSCore * core, const VSAPI * vsapi) {
    std::unique_ptr<NNEDI3CLData> d = std::make_unique<NNEDI3CLData>();
    int err;

    d->node = vsapi->propGetNode(in, "clip", 0, nullptr);
    d->vi = *vsapi->getVideoInfo(d->node);

    try {
        if (!isConstantFormat(&d->vi) ||
            (d->vi.format->sampleType == stInteger && d->vi.format->bitsPerSample > 16) ||
            (d->vi.format->sampleType == stFloat && d->vi.format->bitsPerSample != 32))
            throw std::string{ "only constant format 8-16 bit integer and 32 bit float input supported" };

        d->field = int64ToIntS(vsapi->propGetInt(in, "field", 0, nullptr));

        d->dh = !!vsapi->propGetInt(in, "dh", 0, &err);

        d->dw = !!vsapi->propGetInt(in, "dw", 0, &err);

        const int m = vsapi->propNumElements(in, "planes");

        for (int i = 0; i < 3; i++)
            d->process[i] = (m <= 0);

        for (int i = 0; i < m; i++) {
            const int n = int64ToIntS(vsapi->propGetInt(in, "planes", i, nullptr));

            if (n < 0 || n >= d->vi.format->numPlanes)
                throw std::string{ "plane index out of range" };

            if (d->process[n])
                throw std::string{ "plane specified twice" };

            d->process[n] = true;
        }

        int nsize = int64ToIntS(vsapi->propGetInt(in, "nsize", 0, &err));
        if (err)
            nsize = 6;

        int nns = int64ToIntS(vsapi->propGetInt(in, "nns", 0, &err));
        if (err)
            nns = 1;

        int qual = int64ToIntS(vsapi->propGetInt(in, "qual", 0, &err));
        if (err)
            qual = 1;

        const int etype = int64ToIntS(vsapi->propGetInt(in, "etype", 0, &err));

        int pscrn = int64ToIntS(vsapi->propGetInt(in, "pscrn", 0, &err));
        if (err)
            pscrn = (d->vi.format->sampleType == stInteger) ? 2 : 1;

        int device_id = int64ToIntS(vsapi->propGetInt(in, "device", 0, &err));
        if (err)
            device_id = -1;

        if (d->field < 0 || d->field > 3)
            throw std::string{ "field must be 0, 1, 2 or 3" };

        if (!d->dh && (d->vi.height & 1))
            throw std::string{ "height must be mod 2 when dh=False" };

        if (d->dh && d->field > 1)
            throw std::string{ "field must be 0 or 1 when dh=True" };

        if (d->dw && d->field > 1)
            throw std::string{ "field must be 0 or 1 when dw=True" };

        if (nsize < 0 || nsize > 6)
            throw std::string{ "nsize must be 0, 1, 2, 3, 4, 5 or 6" };

        if (nns < 0 || nns > 4)
            throw std::string{ "nns must be 0, 1, 2, 3 or 4" };

        if (qual < 1 || qual > 2)
            throw std::string{ "qual must be 1 or 2" };

        if (etype < 0 || etype > 1)
            throw std::string{ "etype must be 0 or 1" };

        if (d->vi.format->sampleType == stInteger) {
            if (pscrn < 1 || pscrn > 2)
                throw std::string{ "pscrn must be 1 or 2" };
        } else {
            if (pscrn != 1)
                throw std::string{ "pscrn must be 1 for float input" };
        }

        if (device_id >= static_cast<int>(compute::system::device_count()))
            throw std::string{ "device index out of range" };

        if (!!vsapi->propGetInt(in, "list_device", 0, &err)) {
            const auto devices = compute::system::devices();
            std::string text;

            for (size_t i = 0; i < devices.size(); i++)
                text += std::to_string(i) + ": " + devices[i].name() + " (" + devices[i].platform().name() + ")" + "\n";

            VSMap * args = vsapi->createMap();
            vsapi->propSetNode(args, "clip", d->node, paReplace);
            vsapi->freeNode(d->node);
            vsapi->propSetData(args, "text", text.c_str(), -1, paReplace);

            VSMap * ret = vsapi->invoke(vsapi->getPluginById("com.vapoursynth.text", core), "Text", args);
            if (vsapi->getError(ret)) {
                vsapi->setError(out, vsapi->getError(ret));
                vsapi->freeMap(args);
                vsapi->freeMap(ret);
                return;
            }

            d->node = vsapi->propGetNode(ret, "clip", 0, nullptr);
            vsapi->freeMap(args);
            vsapi->freeMap(ret);
            vsapi->propSetNode(out, "clip", d->node, paReplace);
            vsapi->freeNode(d->node);
            return;
        }

        compute::device device = compute::system::default_device();
        if (device_id > -1)
            device = compute::system::devices().at(device_id);
        compute::context context{ device };
        d->queue = compute::command_queue{ context, device };

        if (!!vsapi->propGetInt(in, "info", 0, &err)) {
            std::string text{ "=== Platform Info ===\n" };
            const auto platform = device.platform();
            text += "Profile: " + platform.get_info<CL_PLATFORM_PROFILE>() + "\n";
            text += "Version: " + platform.get_info<CL_PLATFORM_VERSION>() + "\n";
            text += "Name: " + platform.get_info<CL_PLATFORM_NAME>() + "\n";
            text += "Vendor: " + platform.get_info<CL_PLATFORM_VENDOR>() + "\n";

            text += "\n";

            text += "=== Device Info ===\n";
            text += "Name: " + device.get_info<CL_DEVICE_NAME>() + "\n";
            text += "Vendor: " + device.get_info<CL_DEVICE_VENDOR>() + "\n";
            text += "Profile: " + device.get_info<CL_DEVICE_PROFILE>() + "\n";
            text += "Version: " + device.get_info<CL_DEVICE_VERSION>() + "\n";
            text += "Max compute units: " + std::to_string(device.get_info<CL_DEVICE_MAX_COMPUTE_UNITS>()) + "\n";
            text += "Max work-group size: " + std::to_string(device.get_info<CL_DEVICE_MAX_WORK_GROUP_SIZE>()) + "\n";
            const auto max_work_item_sizes = device.get_info<CL_DEVICE_MAX_WORK_ITEM_SIZES>();
            text += "Max work-item sizes: " + std::to_string(max_work_item_sizes[0]) + ", " + std::to_string(max_work_item_sizes[1]) + ", " + std::to_string(max_work_item_sizes[2]) + "\n";
            text += "2D image max width: " + std::to_string(device.get_info<CL_DEVICE_IMAGE2D_MAX_WIDTH>()) + "\n";
            text += "2D image max height: " + std::to_string(device.get_info<CL_DEVICE_IMAGE2D_MAX_HEIGHT>()) + "\n";
            text += "Image support: " + std::string{ device.get_info<CL_DEVICE_IMAGE_SUPPORT>() ? "CL_TRUE" : "CL_FALSE" } +"\n";
            const auto global_mem_cache_type = device.get_info<CL_DEVICE_GLOBAL_MEM_CACHE_TYPE>();
            if (global_mem_cache_type == CL_NONE)
                text += "Global memory cache type: CL_NONE\n";
            else if (global_mem_cache_type == CL_READ_ONLY_CACHE)
                text += "Global memory cache type: CL_READ_ONLY_CACHE\n";
            else if (global_mem_cache_type == CL_READ_WRITE_CACHE)
                text += "Global memory cache type: CL_READ_WRITE_CACHE\n";
            text += "Global memory cache size: " + std::to_string(device.get_info<CL_DEVICE_GLOBAL_MEM_CACHE_SIZE>() / 1024) + " KB\n";
            text += "Global memory size: " + std::to_string(device.get_info<CL_DEVICE_GLOBAL_MEM_SIZE>() / (1024 * 1024)) + " MB\n";
            text += "Max constant buffer size: " + std::to_string(device.get_info<CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE>() / 1024) + " KB\n";
            text += "Max constant arguments: " + std::to_string(device.get_info<CL_DEVICE_MAX_CONSTANT_ARGS>()) + "\n";
            text += "Local memory type: " + std::string{ device.get_info<CL_DEVICE_LOCAL_MEM_TYPE>() == CL_LOCAL ? "CL_LOCAL" : "CL_GLOBAL" } +"\n";
            text += "Local memory size: " + std::to_string(device.get_info<CL_DEVICE_LOCAL_MEM_SIZE>() / 1024) + " KB\n";
            text += "Available: " + std::string{ device.get_info<CL_DEVICE_AVAILABLE>() ? "CL_TRUE" : "CL_FALSE" } +"\n";
            text += "Compiler available: " + std::string{ device.get_info<CL_DEVICE_COMPILER_AVAILABLE>() ? "CL_TRUE" : "CL_FALSE" } +"\n";
            text += "OpenCL C version: " + device.get_info<CL_DEVICE_OPENCL_C_VERSION>() + "\n";
            text += "Linker available: " + std::string{ device.get_info<CL_DEVICE_LINKER_AVAILABLE>() ? "CL_TRUE" : "CL_FALSE" } +"\n";
            text += "Image max buffer size: " + std::to_string(device.get_info<size_t>(CL_DEVICE_IMAGE_MAX_BUFFER_SIZE) / 1024) + " KB";

            VSMap * args = vsapi->createMap();
            vsapi->propSetNode(args, "clip", d->node, paReplace);
            vsapi->freeNode(d->node);
            vsapi->propSetData(args, "text", text.c_str(), -1, paReplace);

            VSMap * ret = vsapi->invoke(vsapi->getPluginById("com.vapoursynth.text", core), "Text", args);
            if (vsapi->getError(ret)) {
                vsapi->setError(out, vsapi->getError(ret));
                vsapi->freeMap(args);
                vsapi->freeMap(ret);
                return;
            }

            d->node = vsapi->propGetNode(ret, "clip", 0, nullptr);
            vsapi->freeMap(args);
            vsapi->freeMap(ret);
            vsapi->propSetNode(out, "clip", d->node, paReplace);
            vsapi->freeNode(d->node);
            return;
        }

        if (d->field > 1) {
            if (d->vi.numFrames > INT_MAX / 2)
                throw std::string{ "resulting clip is too long" };
            d->vi.numFrames *= 2;

            muldivRational(&d->vi.fpsNum, &d->vi.fpsDen, 2, 1);
        }

        if (d->dh)
            d->vi.height *= 2;

        if (d->dw)
            d->vi.width *= 2;

        const int peak = (1 << d->vi.format->bitsPerSample) - 1;

        const std::string pluginPath{ vsapi->getPluginPath(vsapi->getPluginById("com.holywu.nnedi3cl", core)) };
        std::string weightsPath{ pluginPath.substr(0, pluginPath.find_last_of('/')) + "/nnedi3_weights.bin" };

        FILE * weightsFile = nullptr;
#ifdef _WIN32
        const int requiredSize = MultiByteToWideChar(CP_UTF8, 0, weightsPath.c_str(), -1, nullptr, 0);
        std::unique_ptr<wchar_t[]> wbuffer = std::make_unique<wchar_t[]>(requiredSize);
        MultiByteToWideChar(CP_UTF8, 0, weightsPath.c_str(), -1, wbuffer.get(), requiredSize);
        weightsFile = _wfopen(wbuffer.get(), L"rb");
#else
        weightsFile = std::fopen(weightsPath.c_str(), "rb");
#endif

#if !defined(_WIN32) && defined(NNEDI3_DATADIR)
        if (!weightsFile) {
            weightsPath = std::string{ NNEDI3_DATADIR } + "/nnedi3_weights.bin";
            weightsFile = std::fopen(weightsPath.c_str(), "rb");
        }
#endif
        if (!weightsFile)
            throw std::string{ "error opening file " + weightsPath + " (" + std::strerror(errno) + ")" };

        if (std::fseek(weightsFile, 0, SEEK_END)) {
            std::fclose(weightsFile);
            throw std::string{ "error seeking to the end of file " + weightsPath + " (" + std::strerror(errno) + ")" };
        }

        constexpr long correctSize = 13574928; // Version 0.9.4 of the Avisynth plugin
        const long weightsSize = std::ftell(weightsFile);

        if (weightsSize == -1) {
            std::fclose(weightsFile);
            throw std::string{ "error determining the size of file " + weightsPath + " (" + std::strerror(errno) + ")" };
        } else if (weightsSize != correctSize) {
            std::fclose(weightsFile);
            throw std::string{ "incorrect size of file " + weightsPath + ". Should be " + std::to_string(correctSize) + " bytes, but got " + std::to_string(weightsSize) + " bytes instead" };
        }

        std::rewind(weightsFile);

        float * bdata = reinterpret_cast<float *>(malloc(correctSize));
        const size_t bytesRead = std::fread(bdata, 1, correctSize, weightsFile);

        if (bytesRead != correctSize) {
            std::fclose(weightsFile);
            free(bdata);
            throw std::string{ "error reading file " + weightsPath + ". Should read " + std::to_string(correctSize) + " bytes, but read " + std::to_string(bytesRead) + " bytes instead" };
        }

        std::fclose(weightsFile);

        constexpr int dims0 = 49 * 4 + 5 * 4 + 9 * 4;
        constexpr int dims0new = 4 * 65 + 4 * 5;
        const int dims1 = nnsTable[nns] * 2 * (xdiaTable[nsize] * ydiaTable[nsize] + 1);
        int dims1tsize = 0, dims1offset = 0;

        for (int j = 0; j < numNNS; j++) {
            for (int i = 0; i < numNSIZE; i++) {
                if (i == nsize && j == nns)
                    dims1offset = dims1tsize;
                dims1tsize += nnsTable[j] * 2 * (xdiaTable[i] * ydiaTable[i] + 1) * 2;
            }
        }

        float * weights0 = new float[std::max(dims0, dims0new)];
        float * weights1 = new float[dims1 * 2];

        // Adjust prescreener weights
        if (pscrn == 2) { // using new prescreener
            int * offt = reinterpret_cast<int *>(calloc(4 * 64, sizeof(int)));
            for (int j = 0; j < 4; j++) {
                for (int k = 0; k < 64; k++)
                    offt[j * 64 + k] = ((k >> 3) << 5) + ((j & 3) << 3) + (k & 7);
            }

            const float * bdw = bdata + dims0 + dims0new * (pscrn - 2);
            short * ws = reinterpret_cast<short *>(weights0);
            float * wf = reinterpret_cast<float *>(&ws[4 * 64]);
            double mean[4] = { 0.0, 0.0, 0.0, 0.0 };

            // Calculate mean weight of each first layer neuron
            for (int j = 0; j < 4; j++) {
                double cmean = 0.0;
                for (int k = 0; k < 64; k++)
                    cmean += bdw[offt[j * 64 + k]];

                mean[j] = cmean / 64.0;
            }

            const double half = peak / 2.0;

            // Factor mean removal and 1.0/half scaling into first layer weights. scale to int16 range
            for (int j = 0; j < 4; j++) {
                double mval = 0.0;
                for (int k = 0; k < 64; k++)
                    mval = std::max(mval, std::abs((bdw[offt[j * 64 + k]] - mean[j]) / half));

                const double scale = 32767.0 / mval;
                for (int k = 0; k < 64; k++)
                    ws[offt[j * 64 + k]] = roundds(((bdw[offt[j * 64 + k]] - mean[j]) / half) * scale);

                wf[j] = static_cast<float>(mval / 32767.0);
            }

            memcpy(wf + 4, bdw + 4 * 64, (dims0new - 4 * 64) * sizeof(float));
            free(offt);
        } else { // using old prescreener
            double mean[4] = { 0.0, 0.0, 0.0, 0.0 };

            // Calculate mean weight of each first layer neuron
            for (int j = 0; j < 4; j++) {
                double cmean = 0.0;
                for (int k = 0; k < 48; k++)
                    cmean += bdata[j * 48 + k];

                mean[j] = cmean / 48.0;
            }

            const double half = (d->vi.format->sampleType == stInteger ? peak : 1.0) / 2.0;

            // Factor mean removal and 1.0/half scaling into first layer weights
            for (int j = 0; j < 4; j++) {
                for (int k = 0; k < 48; k++)
                    weights0[j * 48 + k] = static_cast<float>((bdata[j * 48 + k] - mean[j]) / half);
            }

            memcpy(weights0 + 4 * 48, bdata + 4 * 48, (dims0 - 4 * 48) * sizeof(float));
        }

        // Adjust prediction weights
        for (int i = 0; i < 2; i++) {
            const float * bdataT = bdata + dims0 + dims0new * 3 + dims1tsize * etype + dims1offset + i * dims1;
            float * weightsT = weights1 + i * dims1;
            const int nnst = nnsTable[nns];
            const int asize = xdiaTable[nsize] * ydiaTable[nsize];
            const int boff = nnst * 2 * asize;
            double * mean = reinterpret_cast<double *>(calloc(asize + 1 + nnst * 2, sizeof(double)));

            // Calculate mean weight of each neuron (ignore bias)
            for (int j = 0; j < nnst * 2; j++) {
                double cmean = 0.0;
                for (int k = 0; k < asize; k++)
                    cmean += bdataT[j * asize + k];

                mean[asize + 1 + j] = cmean / asize;
            }

            // Calculate mean softmax neuron
            for (int j = 0; j < nnst; j++) {
                for (int k = 0; k < asize; k++)
                    mean[k] += bdataT[j * asize + k] - mean[asize + 1 + j];
                mean[asize] += bdataT[boff + j];
            }
            for (int j = 0; j < asize + 1; j++)
                mean[j] /= nnst;

            // Factor mean removal into weights, and remove global offset from softmax neurons
            for (int j = 0; j < nnst * 2; j++) {
                for (int k = 0; k < asize; k++) {
                    const double q = (j < nnst) ? mean[k] : 0.0;
                    weightsT[j * asize + k] = static_cast<float>(bdataT[j * asize + k] - mean[asize + 1 + j] - q);
                }
                weightsT[boff + j] = static_cast<float>(bdataT[boff + j] - (j < nnst ? mean[asize] : 0.0));
            }

            free(mean);
        }

        free(bdata);

        const int xdia = xdiaTable[nsize];
        const int ydia = ydiaTable[nsize];
        const int asize = xdiaTable[nsize] * ydiaTable[nsize];
        const int xdiad2m1 = std::max(xdia, (pscrn == 1) ? 12 : 16) / 2 - 1;
        const int ydiad2m1 = ydia / 2 - 1;
        const int xOffset = (xdia == 8) ? (pscrn == 1 ? 2 : 4) : 0;
        const int inputWidth = std::max(xdia, (pscrn == 1) ? 12 : 16) + 32 - 1;
        const int inputHeight = ydia + 16 - 1;
        const float scaleAsize = 1.0f / asize;
        const float scaleQual = 1.0f / qual;

        d->weights0 = compute::buffer{ context, std::max(dims0, dims0new) * sizeof(cl_float), CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR | CL_MEM_HOST_NO_ACCESS, weights0 };
        d->weights1Buffer = compute::buffer{ context, dims1 * 2 * sizeof(cl_float), CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR | CL_MEM_HOST_NO_ACCESS, weights1 };
        delete[] weights0;
        delete[] weights1;

        if (static_cast<size_t>(dims1 * 2) > device.get_info<size_t>(CL_DEVICE_IMAGE_MAX_BUFFER_SIZE))
            throw std::string{ "the device's image max buffer size is too small. Reduce nsize/nns...or buy a new graphics card" };

        compute::program program;
        try {
            std::ostringstream options;
            options.imbue(std::locale{ "C" });
            options.precision(16);
            options.setf(std::ios::fixed, std::ios::floatfield);
            options << "-cl-denorms-are-zero -cl-fast-relaxed-math -Werror";
            options << " -D QUAL=" << qual;
            if (pscrn == 1) {
                options << " -D PRESCREEN=prescreenOld";
                options << " -D USE_OLD_PSCRN=1";
                options << " -D USE_NEW_PSCRN=0";
            } else {
                options << " -D PRESCREEN=prescreenNew";
                options << " -D USE_OLD_PSCRN=0";
                options << " -D USE_NEW_PSCRN=1";
            }
            options << " -D PSCRN_OFFSET=" << (pscrn == 1 ? 5 : 6);
            options << " -D DIMS1=" << dims1;
            options << " -D NNS=" << nnsTable[nns];
            options << " -D NNS2=" << (nnsTable[nns] * 2);
            options << " -D XDIA=" << xdia;
            options << " -D YDIA=" << ydia;
            options << " -D ASIZE=" << asize;
            options << " -D XDIAD2M1=" << xdiad2m1;
            options << " -D YDIAD2M1=" << ydiad2m1;
            options << " -D X_OFFSET=" << xOffset;
            options << " -D INPUT_WIDTH=" << inputWidth;
            options << " -D INPUT_HEIGHT=" << inputHeight;
            options << " -D SCALE_ASIZE=" << scaleAsize << "f";
            options << " -D SCALE_QUAL=" << scaleQual << "f";
            options << " -D PEAK=" << peak;
            if (!(d->dh || d->dw)) {
                options << " -D Y_OFFSET=" << (ydia - 1);
                options << " -D Y_STEP=2";
                options << " -D Y_STRIDE=32";
            } else {
                options << " -D Y_OFFSET=" << (ydia / 2);
                options << " -D Y_STEP=1";
                options << " -D Y_STRIDE=16";
            }
            program = compute::program::build_with_source(source, context, options.str());
        } catch (const compute::opencl_error & error) {
            throw error.error_string() + "\n" + program.build_log();
        }

        if (d->vi.format->sampleType == stInteger)
            d->kernel = program.create_kernel("filter_uint");
        else
            d->kernel = program.create_kernel("filter_float");

        cl_image_format imageFormat;
        if (d->vi.format->bytesPerSample == 1)
            imageFormat = { CL_R, CL_UNSIGNED_INT8 };
        else if (d->vi.format->bytesPerSample == 2)
            imageFormat = { CL_R, CL_UNSIGNED_INT16 };
        else
            imageFormat = { CL_R, CL_FLOAT };

        d->src = compute::image2d{ context,
                                   static_cast<size_t>(vsapi->getVideoInfo(d->node)->width),
                                   static_cast<size_t>(vsapi->getVideoInfo(d->node)->height),
                                   compute::image_format{ imageFormat },
                                   CL_MEM_READ_ONLY | CL_MEM_HOST_WRITE_ONLY };

        d->dst = compute::image2d{ context,
                                   static_cast<size_t>(std::max(d->vi.width, d->vi.height)),
                                   static_cast<size_t>(std::max(d->vi.width, d->vi.height)),
                                   compute::image_format{ imageFormat },
                                   CL_MEM_READ_WRITE | CL_MEM_HOST_READ_ONLY };

        d->tmp = (d->dh && d->dw) ? compute::image2d{ context,
                                                      static_cast<size_t>(std::max(d->vi.width, d->vi.height)),
                                                      static_cast<size_t>(std::max(d->vi.width, d->vi.height)),
                                                      compute::image_format{ imageFormat },
                                                      CL_MEM_READ_WRITE | CL_MEM_HOST_NO_ACCESS }
                                  : compute::image2d{};

        {
            constexpr cl_image_format format = { CL_R, CL_FLOAT };

            cl_image_desc desc;
            desc.image_type = CL_MEM_OBJECT_IMAGE1D_BUFFER;
            desc.image_width = dims1 * 2;
            desc.image_height = 1;
            desc.image_depth = 1;
            desc.image_array_size = 0;
            desc.image_row_pitch = 0;
            desc.image_slice_pitch = 0;
            desc.num_mip_levels = 0;
            desc.num_samples = 0;
#ifdef BOOST_COMPUTE_CL_VERSION_2_0
            desc.mem_object = d->weights1Buffer.get();
#else
            desc.buffer = d->weights1Buffer.get();
#endif

            cl_int error = 0;

            cl_mem mem = clCreateImage(context, 0, &format, &desc, nullptr, &error);
            if (!mem)
                BOOST_THROW_EXCEPTION(compute::opencl_error(error));

            d->weights1 = mem;
        }
    } catch (const std::string & error) {
        vsapi->setError(out, ("NNEDI3CL: " + error).c_str());
        vsapi->freeNode(d->node);
        return;
    } catch (const compute::no_device_found & error) {
        vsapi->setError(out, (std::string{ "NNEDI3CL: " } + error.what()).c_str());
        vsapi->freeNode(d->node);
        return;
    } catch (const compute::opencl_error & error) {
        vsapi->setError(out, ("NNEDI3CL: " + error.error_string()).c_str());
        vsapi->freeNode(d->node);
        return;
    }

    vsapi->createFilter(in, out, "NNEDI3CL", nnedi3clInit, nnedi3clGetFrame, nnedi3clFree, fmParallelRequests, 0, d.release(), core);
}

//////////////////////////////////////////
// Init

VS_EXTERNAL_API(void) VapourSynthPluginInit(VSConfigPlugin configFunc, VSRegisterFunction registerFunc, VSPlugin * plugin) {
    configFunc("com.holywu.nnedi3cl", "nnedi3cl", "An intra-field only deinterlacer", VAPOURSYNTH_API_VERSION, 1, plugin);
    registerFunc("NNEDI3CL",
                 "clip:clip;"
                 "field:int;"
                 "dh:int:opt;"
                 "dw:int:opt;"
                 "planes:int[]:opt;"
                 "nsize:int:opt;"
                 "nns:int:opt;"
                 "qual:int:opt;"
                 "etype:int:opt;"
                 "pscrn:int:opt;"
                 "device:int:opt;"
                 "list_device:int:opt;"
                 "info:int:opt;",
                 nnedi3clCreate, nullptr, plugin);
}
