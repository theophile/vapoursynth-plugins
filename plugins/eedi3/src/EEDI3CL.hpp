#pragma once

#include "shared.hpp"

#include "vectorclass/vectorclass.h"

#define BOOST_COMPUTE_DEBUG_KERNEL_COMPILATION
#define BOOST_COMPUTE_HAVE_THREAD_LOCAL
#define BOOST_COMPUTE_THREAD_SAFE
#define BOOST_COMPUTE_USE_OFFLINE_CACHE
#include <boost/compute/core.hpp>
#include <boost/compute/utility/dim.hpp>
namespace compute = boost::compute;

struct EEDI3CLData {
    VSNodeRef * node, * sclip;
    VSVideoInfo vi;
    int field, mdis, vcheck;
    bool dh, process[3], ucubic;
    float gamma, vthresh2;
    int peak, vectorSize, tpitch, mdisVector, tpitchVector;
    float rcpVthresh0, rcpVthresh1, rcpVthresh2;
    compute::device device;
    compute::context context;
    compute::program program;
    cl_image_format clImageFormat;
    std::unordered_map<std::thread::id, compute::command_queue> queue;
    std::unordered_map<std::thread::id, compute::kernel> calculateConnectionCosts;
    std::unordered_map<std::thread::id, compute::image2d> src;
    std::unordered_map<std::thread::id, compute::buffer> ccosts;
    std::unordered_map<std::thread::id, float *> pcosts;
    std::unordered_map<std::thread::id, int *> pbackt, fpath, dmap, tline;
    void (*filter)(const VSFrameRef *, const VSFrameRef *, VSFrameRef *, VSFrameRef **, const int, const EEDI3CLData * const VS_RESTRICT, const VSAPI *);
};
