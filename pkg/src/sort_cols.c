/*
Copyright (c) 2016 - 2020, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "sort_cols.h"
void sort_cols(int *p_matrix, int *sortcols, int *p_ck, const int nconds, int foundPI) {
    int temp;
    for (int i = nconds - 1; i >= 0; i--) {
        for (int c1 = 0; c1 < foundPI; c1++) {
            for (int c2 = c1 + 1; c2 < foundPI; c2++) {
                if (p_matrix[sortcols[c1] * nconds + i] < p_matrix[sortcols[c2] * nconds + i]) {
                    temp = sortcols[c2];
                    for (int c3 = c2; c3 > c1; c3--) {
                        sortcols[c3] = sortcols[c3 - 1];
                    }
                    sortcols[c1] = temp;
                }
            }
        }
        bool nonzero = true;
        int zeroidx = 0;
        while(zeroidx < foundPI && nonzero) {
            nonzero = p_matrix[sortcols[zeroidx] * nconds + i];
            zeroidx++;
        }
        zeroidx--;
        for (int c1 = 0; c1 < zeroidx; c1++) {
            for (int c2 = c1 + 1; c2 < zeroidx; c2++) {
                if (p_matrix[sortcols[c1] * nconds + i] > p_matrix[sortcols[c2] * nconds + i]) {
                    temp = sortcols[c2];
                    for (int c3 = c2; c3 > c1; c3--) {
                        sortcols[c3] = sortcols[c3 - 1];
                    }
                    sortcols[c1] = temp;
                }
            }
        }
    }
    for (int c1 = 0; c1 < foundPI; c1++) {
        for (int c2 = c1 + 1; c2 < foundPI; c2++) {
            if (p_ck[sortcols[c1]] > p_ck[sortcols[c2]]) {
                temp = sortcols[c2];
                for (int c3 = c2; c3 > c1; c3--) {
                    sortcols[c3] = sortcols[c3 - 1];
                }
                sortcols[c1] = temp;
            }
        }
    }
}
