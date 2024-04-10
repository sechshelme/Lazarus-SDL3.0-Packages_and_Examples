#include <SDL3/SDL.h>

int main()
{
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_CAMERA);

	int camera_count;
	SDL_CameraDeviceID* camera_ids = SDL_GetCameraDevices(&camera_count);
	if (camera_ids != NULL && camera_count > 0)
	{
		SDL_CameraSpec spec;// = {0};
		spec.format = SDL_PIXELFORMAT_RGBA8888;
		spec.width = 640;
		spec.height = 480;
		spec.interval_numerator = 1;
		spec.interval_denominator = 30;
		SDL_Camera* camera = SDL_OpenCameraDevice(*camera_ids, &spec);
		if (camera != NULL)
		{
		    SDL_Log("gefunden");
		    SDL_Log("w: %i   h: %i", spec.width, spec.height);
			SDL_Delay(3000); // wait for the bug to happen
			SDL_CloseCamera(camera);
		}
	}

	SDL_Quit();
}
