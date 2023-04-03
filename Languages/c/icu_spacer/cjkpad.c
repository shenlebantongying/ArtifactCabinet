/*
gcc ./cjkpad.c -o cjkpad `pkg-config --libs --cflags icu-uc icu-io`
*/
#include <stdio.h>
#include <string.h>
#include <unicode/utext.h>

static UErrorCode err = U_ZERO_ERROR;

void padCjk(const char str[]) {
    char formatted[1000] = {0};

    UText *ut = utext_openUTF8(NULL, str, -1, &err);

    int begin, end;

    int prevUBlock; // previous char's unicode block
    int currUBlock;

    for (UChar32 cp = utext_next32From(ut, 0);
         cp > -1;
         cp = UTEXT_NEXT32(ut)) {
        currUBlock = ublock_getCode(cp);

        begin = utext_getPreviousNativeIndex(ut);
        end = utext_getNativeIndex(ut);

        printf("str[%2i]->str[%2i] ", begin, end);
        printf("%.*s ", end - begin, str + begin);
        printf("U+%X\n", cp);

        if (begin != 0 // not first
            // code block between two character changes
            && currUBlock != prevUBlock
            // one of the previous or current character belong to CJK
            && (prevUBlock == UBLOCK_CJK_UNIFIED_IDEOGRAPHS || currUBlock == UBLOCK_CJK_UNIFIED_IDEOGRAPHS)) {
            strcat(formatted, " ");
        }

        strncat(formatted, &str[begin], end - begin);

        prevUBlock = currUBlock;
    }

    printf("\n%s\n", formatted);

}

int main(void) {

    char str[] = "你好world哈哈nice";

    padCjk(str);
}
