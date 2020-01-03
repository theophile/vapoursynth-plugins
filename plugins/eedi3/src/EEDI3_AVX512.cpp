#ifdef VS_TARGET_CPU_X86
#ifndef __AVX512F__
#define __AVX512F__
#endif

#include "EEDI3.hpp"

template<typename T>
static inline void calculateConnectionCosts(const T * srcp, const bool * bmask, float * ccosts, const int width, const int stride, const EEDI3Data * const VS_RESTRICT d) noexcept {
    const __m512i * src3p = reinterpret_cast<const __m512i *>(srcp) + 12;
    const __m512i * src1p = src3p + stride;
    const __m512i * src1n = src1p + stride;
    const __m512i * src3n = src1n + stride;

    if (d->cost3) {
        for (int x = 0; x < width; x++) {
            if (!bmask || bmask[x]) {
                const int umax = std::min({ x, width - 1 - x, d->mdis });
                for (int u = -umax; u <= umax; u++) {
                    const int u2 = u * 2;
                    const bool s1Flag = (u >= 0 && x >= u2) || (u <= 0 && x < width + u2);
                    const bool s2Flag = (u <= 0 && x >= -u2) || (u >= 0 && x < width - u2);
                    Vec16i s0 = zero_512b(), s1 = zero_512b(), s2 = zero_512b();

                    for (int k = -(d->nrad); k <= d->nrad; k++)
                        s0 += abs(Vec16i().load_a(src3p + x + u + k) - Vec16i().load_a(src1p + x - u + k)) +
                              abs(Vec16i().load_a(src1p + x + u + k) - Vec16i().load_a(src1n + x - u + k)) +
                              abs(Vec16i().load_a(src1n + x + u + k) - Vec16i().load_a(src3n + x - u + k));

                    if (s1Flag) {
                        for (int k = -(d->nrad); k <= d->nrad; k++)
                            s1 += abs(Vec16i().load_a(src3p + x + k) - Vec16i().load_a(src1p + x - u2 + k)) +
                                  abs(Vec16i().load_a(src1p + x + k) - Vec16i().load_a(src1n + x - u2 + k)) +
                                  abs(Vec16i().load_a(src1n + x + k) - Vec16i().load_a(src3n + x - u2 + k));
                    }

                    if (s2Flag) {
                        for (int k = -(d->nrad); k <= d->nrad; k++)
                            s2 += abs(Vec16i().load_a(src3p + x + u2 + k) - Vec16i().load_a(src1p + x + k)) +
                                  abs(Vec16i().load_a(src1p + x + u2 + k) - Vec16i().load_a(src1n + x + k)) +
                                  abs(Vec16i().load_a(src1n + x + u2 + k) - Vec16i().load_a(src3n + x + k));
                    }

                    s1 = s1Flag ? s1 : (s2Flag ? s2 : s0);
                    s2 = s2Flag ? s2 : (s1Flag ? s1 : s0);

                    const Vec16i ip = (Vec16i().load_a(src1p + x + u) + Vec16i().load_a(src1n + x - u) + 1) >> 1; // should use cubic if ucubic=true
                    const Vec16f v = to_float(abs(Vec16i().load_a(src1p + x) - ip) + abs(Vec16i().load_a(src1n + x) - ip));
                    const Vec16f result = mul_add(d->alpha, to_float(s0 + s1 + s2), d->beta * std::abs(u)) + d->remainingWeight * v;
                    result.stream(ccosts + (d->tpitch * x + u) * d->vectorSize);
                }
            }
        }
    } else {
        for (int x = 0; x < width; x++) {
            if (!bmask || bmask[x]) {
                const int umax = std::min({ x, width - 1 - x, d->mdis });
                for (int u = -umax; u <= umax; u++) {
                    Vec16i s = zero_512b();

                    for (int k = -(d->nrad); k <= d->nrad; k++)
                        s += abs(Vec16i().load_a(src3p + x + u + k) - Vec16i().load_a(src1p + x - u + k)) +
                             abs(Vec16i().load_a(src1p + x + u + k) - Vec16i().load_a(src1n + x - u + k)) +
                             abs(Vec16i().load_a(src1n + x + u + k) - Vec16i().load_a(src3n + x - u + k));

                    const Vec16i ip = (Vec16i().load_a(src1p + x + u) + Vec16i().load_a(src1n + x - u) + 1) >> 1; // should use cubic if ucubic=true
                    const Vec16f v = to_float(abs(Vec16i().load_a(src1p + x) - ip) + abs(Vec16i().load_a(src1n + x) - ip));
                    const Vec16f result = mul_add(d->alpha, to_float(s), d->beta * std::abs(u)) + d->remainingWeight * v;
                    result.stream(ccosts + (d->tpitch * x + u) * d->vectorSize);
                }
            }
        }
    }
}

template<>
inline void calculateConnectionCosts(const float * srcp, const bool * bmask, float * ccosts, const int width, const int stride, const EEDI3Data * const VS_RESTRICT d) noexcept {
    const __m512 * src3p = reinterpret_cast<const __m512 *>(srcp) + 12;
    const __m512 * src1p = src3p + stride;
    const __m512 * src1n = src1p + stride;
    const __m512 * src3n = src1n + stride;

    if (d->cost3) {
        for (int x = 0; x < width; x++) {
            if (!bmask || bmask[x]) {
                const int umax = std::min({ x, width - 1 - x, d->mdis });
                for (int u = -umax; u <= umax; u++) {
                    const int u2 = u * 2;
                    const bool s1Flag = (u >= 0 && x >= u2) || (u <= 0 && x < width + u2);
                    const bool s2Flag = (u <= 0 && x >= -u2) || (u >= 0 && x < width - u2);
                    Vec16f s0 = zero_16f(), s1 = zero_16f(), s2 = zero_16f();

                    for (int k = -(d->nrad); k <= d->nrad; k++)
                        s0 += abs(Vec16f().load_a(src3p + x + u + k) - Vec16f().load_a(src1p + x - u + k)) +
                              abs(Vec16f().load_a(src1p + x + u + k) - Vec16f().load_a(src1n + x - u + k)) +
                              abs(Vec16f().load_a(src1n + x + u + k) - Vec16f().load_a(src3n + x - u + k));

                    if (s1Flag) {
                        for (int k = -(d->nrad); k <= d->nrad; k++)
                            s1 += abs(Vec16f().load_a(src3p + x + k) - Vec16f().load_a(src1p + x - u2 + k)) +
                                  abs(Vec16f().load_a(src1p + x + k) - Vec16f().load_a(src1n + x - u2 + k)) +
                                  abs(Vec16f().load_a(src1n + x + k) - Vec16f().load_a(src3n + x - u2 + k));
                    }

                    if (s2Flag) {
                        for (int k = -(d->nrad); k <= d->nrad; k++)
                            s2 += abs(Vec16f().load_a(src3p + x + u2 + k) - Vec16f().load_a(src1p + x + k)) +
                                  abs(Vec16f().load_a(src1p + x + u2 + k) - Vec16f().load_a(src1n + x + k)) +
                                  abs(Vec16f().load_a(src1n + x + u2 + k) - Vec16f().load_a(src3n + x + k));
                    }

                    s1 = s1Flag ? s1 : (s2Flag ? s2 : s0);
                    s2 = s2Flag ? s2 : (s1Flag ? s1 : s0);

                    const Vec16f ip = (Vec16f().load_a(src1p + x + u) + Vec16f().load_a(src1n + x - u)) * 0.5f; // should use cubic if ucubic=true
                    const Vec16f v = abs(Vec16f().load_a(src1p + x) - ip) + abs(Vec16f().load_a(src1n + x) - ip);
                    const Vec16f result = mul_add(d->alpha, s0 + s1 + s2, d->beta * std::abs(u)) + d->remainingWeight * v;
                    result.stream(ccosts + (d->tpitch * x + u) * d->vectorSize);
                }
            }
        }
    } else {
        for (int x = 0; x < width; x++) {
            if (!bmask || bmask[x]) {
                const int umax = std::min({ x, width - 1 - x, d->mdis });
                for (int u = -umax; u <= umax; u++) {
                    Vec16f s = zero_16f();

                    for (int k = -(d->nrad); k <= d->nrad; k++)
                        s += abs(Vec16f().load_a(src3p + x + u + k) - Vec16f().load_a(src1p + x - u + k)) +
                             abs(Vec16f().load_a(src1p + x + u + k) - Vec16f().load_a(src1n + x - u + k)) +
                             abs(Vec16f().load_a(src1n + x + u + k) - Vec16f().load_a(src3n + x - u + k));

                    const Vec16f ip = (Vec16f().load_a(src1p + x + u) + Vec16f().load_a(src1n + x - u)) * 0.5f; // should use cubic if ucubic=true
                    const Vec16f v = abs(Vec16f().load_a(src1p + x) - ip) + abs(Vec16f().load_a(src1n + x) - ip);
                    const Vec16f result = mul_add(d->alpha, s, d->beta * std::abs(u)) + d->remainingWeight * v;
                    result.stream(ccosts + (d->tpitch * x + u) * d->vectorSize);
                }
            }
        }
    }
}

template<typename T1, typename T2>
void filter_avx512(const VSFrameRef * src, const VSFrameRef * scp, const VSFrameRef * mclip, VSFrameRef * mcp, VSFrameRef * dst, VSFrameRef ** pad,
                   const int field_n, const EEDI3Data * const VS_RESTRICT d, const VSAPI * vsapi) noexcept {
    for (int plane = 0; plane < d->vi.format->numPlanes; plane++) {
        if (d->process[plane]) {
            copyPad<T1>(src, pad[plane], plane, 1 - field_n, d->dh, vsapi);

            const int srcWidth = vsapi->getFrameWidth(pad[plane], 0);
            const int dstWidth = vsapi->getFrameWidth(dst, plane);
            const int srcHeight = vsapi->getFrameHeight(pad[plane], 0);
            const int dstHeight = vsapi->getFrameHeight(dst, plane);
            const int srcStride = vsapi->getStride(pad[plane], 0) / sizeof(T1);
            const int dstStride = vsapi->getStride(dst, plane) / sizeof(T1);
            const T1 * _srcp = reinterpret_cast<const T1 *>(vsapi->getReadPtr(pad[plane], 0)) + 12;
            T1 * VS_RESTRICT _dstp = reinterpret_cast<T1 *>(vsapi->getWritePtr(dst, plane));

            const uint8_t * maskp = nullptr;
            if (d->mclip) {
                copyMask(mclip, mcp, plane, field_n, d->dh, vsapi);
                maskp = vsapi->getReadPtr(mcp, plane);
            }

            const auto threadId = std::this_thread::get_id();
            T2 * srcVector = reinterpret_cast<T2 *>(d->srcVector.at(threadId));
            uint8_t * _mskVector = d->mskVector.at(threadId);
            bool * bmask = d->bmask.at(threadId);
            float * ccosts = d->ccosts.at(threadId) + d->mdisVector;
            float * pcosts = d->pcosts.at(threadId) + d->mdisVector;
            int * _pbackt = d->pbackt.at(threadId) + d->mdisVector;
            int * fpath = d->fpath.at(threadId);
            int * _dmap = d->dmap.at(threadId);
            int * tline = d->tline.at(threadId);

            vs_bitblt(_dstp + dstStride * (1 - field_n), vsapi->getStride(dst, plane) * 2,
                      _srcp + srcStride * (4 + 1 - field_n), vsapi->getStride(pad[plane], 0) * 2,
                      dstWidth * sizeof(T1), dstHeight / 2);

            _srcp += srcStride * 4;
            _dstp += dstStride * field_n;

            for (int y = field_n; y < dstHeight; y += 2 * d->vectorSize) {
                const int off = (y - field_n) >> 1;

                prepareLines<T1, T2>(_srcp + srcStride * (1 - field_n), srcVector, dstWidth, (dstHeight + field_n) >> 1, srcStride * 2, srcWidth, off + field_n, d->vectorSize);

                if (bmask) {
                    prepareMask(maskp, _mskVector, dstWidth, (dstHeight + field_n) >> 1, vsapi->getStride(mcp, plane), off, d->vectorSize);

                    const int64_t * mskVector = reinterpret_cast<const int64_t *>(_mskVector);
                    const int minmdis = std::min(dstWidth, d->mdis);
                    int last = -666999;

                    for (int x = 0; x < minmdis; x++) {
                        if (mskVector[x * 2] || mskVector[x * 2 + 1])
                            last = x + d->mdis;
                    }

                    for (int x = 0; x < dstWidth - minmdis; x++) {
                        if (mskVector[(x + d->mdis) * 2] || mskVector[(x + d->mdis) * 2 + 1])
                            last = x + d->mdis * 2;

                        bmask[x] = (x <= last);
                    }

                    for (int x = dstWidth - minmdis; x < dstWidth; x++)
                        bmask[x] = (x <= last);

                    memset(ccosts - d->mdisVector, 0, dstWidth * d->tpitchVector * sizeof(float));
                }

                calculateConnectionCosts<T2>(srcVector, bmask, ccosts, dstWidth, srcWidth, d);

                // calculate path costs
                Vec16f().load_a(ccosts).store_a(pcosts);
                for (int x = 1; x < dstWidth; x++) {
                    const float * tT = ccosts + d->tpitchVector * x;
                    const float * ppT = pcosts + d->tpitchVector * (x - 1);
                    float * pT = pcosts + d->tpitchVector * x;
                    int * piT = _pbackt + d->tpitchVector * (x - 1);

                    if (bmask && !bmask[x]) {
                        if (x == 1) {
                            const int umax = std::min({ x, dstWidth - 1 - x, d->mdis });
                            memcpy(pT - umax * d->vectorSize, tT - umax * d->vectorSize, (umax * 2 + 1) * d->vectorSize * sizeof(float));
                            memset(piT - d->mdisVector, 0, d->tpitchVector * sizeof(int));
                        } else {
                            memcpy(pT - d->mdisVector, ppT - d->mdisVector, d->tpitchVector * sizeof(float));
                            memcpy(piT - d->mdisVector, piT - d->mdisVector - d->tpitchVector, d->tpitchVector * sizeof(int));
                            const int pumax = std::min(x - 1, dstWidth - x);
                            if (pumax < d->mdis) {
                                Vec16i(1 - pumax).stream(piT - pumax * d->vectorSize);
                                Vec16i(pumax - 1).stream(piT + pumax * d->vectorSize);
                            }
                        }
                    } else {
                        const int umax = std::min({ x, dstWidth - 1 - x, d->mdis });
                        const int umax2 = std::min({ x - 1, dstWidth - x, d->mdis });
                        for (int u = -umax; u <= umax; u++) {
                            Vec16i idx = zero_512b();
                            Vec16f bval = FLT_MAX;

                            for (int v = std::max(-umax2, u - 1); v <= std::min(umax2, u + 1); v++) {
                                const Vec16f z = Vec16f().load_a(ppT + v * d->vectorSize) + d->gamma * std::abs(u - v);
                                const Vec16f ccost = min(z, FLT_MAX * 0.9f);
                                idx = select(Vec16ib(ccost < bval), v, idx);
                                bval = min(ccost, bval);
                            }

                            const Vec16f z = bval + Vec16f().load_a(tT + u * d->vectorSize);
                            min(z, FLT_MAX * 0.9f).store_a(pT + u * d->vectorSize);
                            idx.stream(piT + u * d->vectorSize);
                        }
                    }
                }

                for (int vs = 0; vs < d->vectorSize; vs++) {
                    const int realY = field_n + 2 * (off + vs);
                    if (realY >= dstHeight)
                        break;

                    const T1 * srcp = _srcp + srcStride * realY;
                    T1 * dstp = _dstp + dstStride * 2 * (off + vs);
                    int * dmap = _dmap + dstWidth * (off + vs);

                    const T1 * src3p = srcp - srcStride * 3;
                    const T1 * src1p = srcp - srcStride;
                    const T1 * src1n = srcp + srcStride;
                    const T1 * src3n = srcp + srcStride * 3;

                    const int * pbackt = _pbackt + vs;

                    // backtrack
                    fpath[dstWidth - 1] = 0;
                    for (int x = dstWidth - 2; x >= 0; x--)
                        fpath[x] = pbackt[(d->tpitch * x + fpath[x + 1]) * d->vectorSize];

                    interpolate<T1>(src3p, src1p, src1n, src3n, bmask, fpath, dmap, dstp, dstWidth, d->ucubic, d->peak);
                }
            }

            _srcp += srcStride * field_n;

            if (d->vcheck) {
                const T1 * scpp = nullptr;
                if (d->sclip)
                    scpp = reinterpret_cast<const T1 *>(vsapi->getReadPtr(scp, plane)) + dstStride * field_n;

                vCheck<T1>(_srcp, scpp, _dstp, _dmap, tline, field_n, dstWidth, srcHeight, srcStride, dstStride, d->vcheck, d->vthresh2, d->rcpVthresh0, d->rcpVthresh1, d->rcpVthresh2, d->peak);
            }
        }
    }
}

template void filter_avx512<uint8_t, int>(const VSFrameRef *, const VSFrameRef *, const VSFrameRef *, VSFrameRef *, VSFrameRef *, VSFrameRef **, const int, const EEDI3Data * const VS_RESTRICT, const VSAPI *) noexcept;
template void filter_avx512<uint16_t, int>(const VSFrameRef *, const VSFrameRef *, const VSFrameRef *, VSFrameRef *, VSFrameRef *, VSFrameRef **, const int, const EEDI3Data * const VS_RESTRICT, const VSAPI *) noexcept;
template void filter_avx512<float, float>(const VSFrameRef *, const VSFrameRef *, const VSFrameRef *, VSFrameRef *, VSFrameRef *, VSFrameRef **, const int, const EEDI3Data * const VS_RESTRICT, const VSAPI *) noexcept;
#endif
