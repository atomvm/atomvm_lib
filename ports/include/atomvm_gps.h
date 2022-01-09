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

#ifndef __ATOMVM_GPS_H__
#define __ATOMVM_GPS_H__

#include "context.h"
#include "globalcontext.h"
#include "term.h"

void atomvm_gps_init(GlobalContext *global);
Context *atomvm_gps_create_port(GlobalContext *global, term opts);

#endif
