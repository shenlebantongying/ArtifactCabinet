#include <SDL3/SDL.h>
#include <cstdio>
#include <cstdlib>

void check_bool(bool x, int line)
{
    if (!x) {
        printf("%d -> %s\n", line, SDL_GetError());
        exit(1);
    }
}

void check_null(void* x, int line)
{

    if (x == nullptr) {
        printf("%d -> %s\n", line, SDL_GetError());
        exit(1);
    }
}

int main(int, char**)
{
    bool ret;

    ret = SDL_Init(SDL_INIT_VIDEO);
    check_bool(ret, __LINE__);
    SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "x11");

    auto* sdl_gpu_device = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV, true, "vulkan");
    check_null(sdl_gpu_device, __LINE__);

    auto* sdl_window = SDL_CreateWindow("Hi", 500, 500, NULL);
    check_null(sdl_window, __LINE__);

    ret = SDL_ClaimWindowForGPUDevice(sdl_gpu_device, sdl_window);
    check_bool(ret, __LINE__);

    bool running = true;
    while (running) {
        printf("Loop\n");
        SDL_Event sdl_event;

        while (SDL_PollEvent(&sdl_event)) {
            if (sdl_event.type == SDL_EVENT_QUIT) {
                running = false;
            }
        }

        SDL_GPUCommandBuffer* commandBuff = SDL_AcquireGPUCommandBuffer(sdl_gpu_device);

        SDL_GPUTexture* swapchainTexture = NULL;

        ret = SDL_WaitAndAcquireGPUSwapchainTexture(commandBuff, sdl_window, &swapchainTexture, NULL, NULL);
        check_bool(ret, __LINE__);

        auto color = SDL_FColor {
            .r = 1.0f,
            .g = 0.0f,
            .b = 0.0f,
            .a = 1.0f,
        };

        SDL_GPUColorTargetInfo colorTargetInfoo {};

        colorTargetInfoo.texture = swapchainTexture;
        colorTargetInfoo.clear_color = color;
        colorTargetInfoo.load_op = SDL_GPU_LOADOP_CLEAR;
        colorTargetInfoo.store_op = SDL_GPU_STOREOP_STORE;

        SDL_GPURenderPass* renderPass = SDL_BeginGPURenderPass(commandBuff, &colorTargetInfoo, 1, NULL);
        SDL_EndGPURenderPass(renderPass);
        SDL_SubmitGPUCommandBuffer(commandBuff);
    }

    SDL_ReleaseWindowFromGPUDevice(sdl_gpu_device, sdl_window);
    SDL_DestroyGPUDevice(sdl_gpu_device);
    SDL_DestroyWindow(sdl_window);
    SDL_Quit();

    return 0;
}
