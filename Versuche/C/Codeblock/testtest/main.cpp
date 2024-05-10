#include <iostream>
#include <thread>
#include <SDL2/SDL.h>

#ifdef _WIN32
	#define NOMINMAX 1
	#define WIN32_LEAN_AND_MEAN
	#include <Windows.h>
#endif // _WIN32

class Renderer
{
public:
    Renderer(SDL_Window* window);
    ~Renderer();
    void stop();

private:
    void renderJob();
    void render();

    SDL_GLContext mContext;
    SDL_Renderer* mRenderer;
    SDL_Window* mWindow;
    std::thread *mThread;
    SDL_mutex *mLock;
    SDL_cond *mCond;
    bool mFirstRun, mKeepRunning;
};

Renderer::Renderer(SDL_Window* window)
    : mWindow(window),
      mRenderer(nullptr),
      mLock(nullptr),
      mCond(nullptr),
      mFirstRun(true),
      mKeepRunning(true)
{
    mContext = SDL_GL_GetCurrentContext();

    SDL_GL_MakeCurrent(window, nullptr);

    // Create the mutex/condition needed to signal
    // when the renderer has been initialized.
    mLock = SDL_CreateMutex();
    mCond = SDL_CreateCond();

    // Start the rendering thread.
    mThread = new std::thread (&Renderer::renderJob, this);

    // Wait for the renderer to be initialized.
    SDL_LockMutex(mLock);
    while (mFirstRun)
        SDL_CondWait(mCond, mLock);
    SDL_UnlockMutex(mLock);
}


Renderer::~Renderer()
{
    SDL_DestroyCond(mCond);
    SDL_DestroyMutex(mLock);
    delete mThread;
}


void Renderer::stop()
{
    mKeepRunning = false;
    mThread->join();
}


void Renderer::render()
{
    // Clear
    if (SDL_SetRenderDrawColor(mRenderer, 0, 0, 0, 255) < 0)
    {
        std::cerr << "Error clearing: " << SDL_GetError() << std::endl;
    }

    // Draw
    SDL_Rect rect;
    rect.x = 10;
    rect.y = 10;
    rect.w = 50;
    rect.h = 50;

    SDL_SetRenderDrawColor(mRenderer, 255, 0, 0, 255);

    if (SDL_RenderDrawRect(mRenderer, &rect) < 0)
    {
        std::cerr << "Error drawing: " << SDL_GetError() << std::endl;
    }

    // Display
    SDL_RenderPresent(mRenderer);

    // Cap framerate at ~50 Hz.
    SDL_Delay(20);
}

void Renderer::renderJob()
{
    while (mKeepRunning)
    {
        if (mFirstRun)
        {
            SDL_GL_MakeCurrent(mWindow, mContext);

            mRenderer = SDL_CreateRenderer(mWindow, -1, SDL_RENDERER_ACCELERATED);

            // Note that the renderer has been initialized.
            SDL_LockMutex(mLock);
            mFirstRun = false;
            SDL_CondSignal(mCond);
            SDL_UnlockMutex(mLock);
        }
        if (mRenderer != nullptr)
        {
            render();
        }
    }
}

int main(int argc, char **argv)
{
    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);

    SDL_Window *window = SDL_CreateWindow("Test", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 500, 500, SDL_WINDOW_RESIZABLE);

    Renderer r(window);

    while (window)
    {
        SDL_Event event;

        if (SDL_WaitEvent(&event))
        {
            if (event.type == SDL_QUIT)
            {
                r.stop();
                SDL_DestroyWindow(window);
                window = nullptr;
            }
        }
    }

    SDL_Quit();

    return 0;
}

#ifdef __MINGW32__

// MinGW expects a WinMain().
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevIns, LPSTR lpszArgument, int iShow)
{
	return main(0, nullptr);
}

#endif // __MINGW32__
