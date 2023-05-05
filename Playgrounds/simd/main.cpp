#include <immintrin.h> //include everything for SIMD
#include <iostream>


int main() {

    __m128 m1 = _mm_set_ps(1.0f,2.0f,3.0f,4.0f);
    __m128 m2 = _mm_set_ps(1.1f,2.2f,3.3f,4.4f);
    __m128 m3 = _mm_add_ps(m1,m2);

    int32_t one[4]={1,2,3,4};

    std::cout<<one[0]<<std::endl;
    std::cout<<m3[0]<<std::endl;

    return 0;
}
