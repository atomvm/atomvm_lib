//
// Copyright (c) 2020 dushin.net
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

#include <stdlib.h>

#include <esp_log.h>
#include <esp_attr.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <term.h>
#include <memory.h>

//#define ENABLE_TRACE
#include "trace.h"

#define TAG "atomvm_lib"

RTC_DATA_ATTR size_t data_len = 0;
RTC_DATA_ATTR char *data[CONFIG_RTC_MEMORY_SIZE];

static term nif_set_rtc_memory(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term binary = argv[0];
    VALIDATE_VALUE(binary, term_is_binary);

    size_t binary_len = term_binary_size(binary);
    if (CONFIG_RTC_MEMORY_SIZE < binary_len) {
        RAISE_ERROR(BADARG_ATOM);
    }
    data_len = binary_len;
    memcpy(data, term_binary_data(binary), binary_len);

    return OK_ATOM;
}

static term nif_get_rtc_memory(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(data_len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(data, data_len, ctx);
}


static const struct Nif set_rtc_memory_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_set_rtc_memory
};
static const struct Nif get_rtc_memory_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_get_rtc_memory
};


//
// Component Nif Entrypoints
//

void atomvm_lib_init(GlobalContext *global)
{
    // no-op
}

const struct Nif *atomvm_lib_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...", nifname);
    if (strcmp("atomvm_lib:set_rtc_memory/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &set_rtc_memory_nif;
    }
    if (strcmp("atomvm_lib:get_rtc_memory/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &get_rtc_memory_nif;
    }
    return NULL;
}
