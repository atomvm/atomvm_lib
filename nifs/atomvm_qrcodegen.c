//
// Copyright (c) 2021 dushin.net
// All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <port.h>
#include <term.h>
#include <esp_err.h>
#include <esp_log.h>

#include "atomvm_qrcodegen.h"
#include "qrcodegen.h"

// #define ENABLE_TRACE
#include <trace.h>

#define TAG "atomvm_qrcodegen"

static const char *const low_atom =         "\x3" "low";
static const char *const medium_atom =      "\x6" "medium";
static const char *const quartile_atom =    "\x8" "quartile";
static const char *const high_atom =        "\x4" "high";

static bool is_boolean(term t)
{
    return t == TRUE_ATOM || t == FALSE_ATOM;
}

static term nif_qrcodegen_encode_text(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term data_term = argv[0];
    VALIDATE_VALUE(data_term, term_is_binary);
    term ecc_term = argv[1];
    VALIDATE_VALUE(ecc_term, term_is_atom);
    term min_version_term = argv[2];
    VALIDATE_VALUE(min_version_term, term_is_integer);
    term max_version_term = argv[3];
    VALIDATE_VALUE(max_version_term, term_is_integer);
    term mask_term = argv[4];
    VALIDATE_VALUE(mask_term, term_is_atom);
    term boost_ecl_term = argv[5];
    VALIDATE_VALUE(boost_ecl_term, is_boolean);

    size_t max_version = term_to_int(max_version_term);
    if (max_version < 1 || 40 < max_version) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t min_version = term_to_int(min_version_term);
    if (min_version < 1 || 40 < min_version) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (max_version < min_version) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t buf_size = qrcodegen_BUFFER_LEN_FOR_VERSION(max_version);
	uint8_t *tmp = malloc(buf_size);
    if (IS_NULL_PTR(tmp)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    char *text = interop_binary_to_string(data_term);

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(buf_size)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term qr_code = term_create_uninitialized_binary(buf_size, ctx);
    uint8_t *buf = (uint8_t *) term_binary_data(qr_code);

	bool ok = qrcodegen_encodeText(
        text, tmp, buf,
        qrcodegen_Ecc_LOW,
		min_version, max_version,
        qrcodegen_Mask_AUTO,
        boost_ecl_term == TRUE_ATOM
    );
    free(tmp);
    free(text);

    if (!ok) {
        return ERROR_ATOM;
    }
    return qr_code;
}

static term nif_qrcodegen_encode_bin(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    // TODO

    return ERROR_ATOM;
}


static term nif_qrcodegen_get_side(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term qrcode_term = argv[0];
    VALIDATE_VALUE(qrcode_term, term_is_binary);

    const uint8_t *qrcode = (const uint8_t *) term_binary_data(qrcode_term);
    avm_int_t side = (avm_int_t) qrcodegen_getSize(qrcode);

    return term_from_int(side);
}


static term nif_qrcodegen_get_pixel(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term qrcode_term = argv[0];
    VALIDATE_VALUE(qrcode_term, term_is_binary);
    term x_term = argv[1];
    VALIDATE_VALUE(x_term, term_is_integer);
    term y_term = argv[2];
    VALIDATE_VALUE(y_term, term_is_integer);

    const uint8_t *qrcode = (const uint8_t *) term_binary_data(qrcode_term);

    return qrcodegen_getModule(qrcode, term_to_int(x_term), term_to_int(y_term)) ? TRUE_ATOM : FALSE_ATOM;
}


static inline void write_bit(uint8_t *buf, size_t bit)
{
    buf[bit / 8] |= 1 << (7 - (bit % 8));
}

static int qrcodegen_to_bitmap(uint8_t *buf, size_t side, const uint8_t *qrcode)
{
    size_t bit = 0;
    for (size_t y = 0;  y < side;  ++y) {
        for (size_t x = 0;  x < side;  ++x) {
            if (qrcodegen_getModule(qrcode, x, y)) {
                // printf(".");
                write_bit(buf, bit);
            } else {
                // printf(" ");
            }
            bit++;
        }
        // printf("\n");
    }
    return 0;
}

static term nif_qrcodegen_to_bitmap(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term qrcode_term = argv[0];
    VALIDATE_VALUE(qrcode_term, term_is_binary);

    const uint8_t *qrcode = (const uint8_t *) term_binary_data(qrcode_term);
    size_t side = (size_t) qrcodegen_getSize(qrcode);
    size_t s2 = side * side;

    size_t buf_size = (s2 / 8) + ((s2 % 8) == 0 ? 0 : 1);
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(buf_size) + 3) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term bitmap = term_create_uninitialized_binary(buf_size, ctx);
    uint8_t *buf = (uint8_t *) term_binary_data(bitmap);
    memset(buf, 0, buf_size);

    // re-read QRCode (in case of GC)
    qrcode = (const uint8_t *) term_binary_data(argv[0]);
    if (qrcodegen_to_bitmap(buf, side, qrcode) == -1) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return port_create_tuple2(ctx, term_from_int(side), bitmap);
}

//
// Nif integration
//

static const struct Nif qrcodegen_encode_text_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_qrcodegen_encode_text
};
static const struct Nif qrcodegen_encode_bin_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_qrcodegen_encode_bin
};
static const struct Nif qrcodegen_get_side_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_qrcodegen_get_side
};
static const struct Nif qrcodegen_get_pixel_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_qrcodegen_get_pixel
};
static const struct Nif qrcodegen_to_bitmap_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_qrcodegen_to_bitmap
};


void atomvm_qrcodegen_init(GlobalContext *global)
{
    // no-op
}
const struct Nif *atomvm_qrcodegen_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...", nifname);
    if (strcmp("qrcodegen:nif_encode_text/6", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &qrcodegen_encode_text_nif;
    }
    if (strcmp("qrcodegen:nif_encode_bin/6", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &qrcodegen_encode_bin_nif;
    }
    if (strcmp("qrcodegen:nif_get_side/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &qrcodegen_get_side_nif;
    }
    if (strcmp("qrcodegen:nif_get_pixel/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &qrcodegen_get_pixel_nif;
    }
    if (strcmp("qrcodegen:nif_to_bitmap/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &qrcodegen_to_bitmap_nif;
    }
    return NULL;
}
