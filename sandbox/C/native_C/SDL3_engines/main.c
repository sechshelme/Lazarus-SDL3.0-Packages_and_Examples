/*
  Linux:
  g++ main.c -o main -lSDL3 -lSDL3_image
  
  Windows:
  x86_64-w64-mingw32-gcc main.c -o main.exe -lSDL3 -I/usr/local/include -L/usr/local/bin
*/

#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

#include <vector>
#include <string>
#include <stdlib.h>
#include <math.h>

std::string imagePath = "mauer.bmp";

class object
{
	public:
	object(SDL_Renderer * screen)
	{
		renderer = screen;
		speed.x = 0;
		speed.y = 0;
		maxY = 900;
		hp = 70;
		pos = {0, 0, 10, 10};
	}

	virtual void update()
	{
		hp --;
		if(pos.y + pos.h > maxY)
		{
			onGround = true;
			if(speed.y > 0)
			{
				pos.y = maxY - pos.h;
			}
		}
		else
		{
			onGround = false;
			speed.y += gravity;
		}
		pos.y += speed.y;
		pos.x += speed.x;
	}

	bool isOnGround()
	{
		return onGround;
	}

	virtual void setPos(float x, float y)
	{
		pos.x = x;
		pos.y = y;
	}

	virtual void setImage(SDL_Texture * src)
	{
		image = src;
	}

	bool isAlive()
	{
		return hp > 0;
	}

	virtual void draw()
	{
		SDL_RenderTexture(renderer, image, NULL, &pos);
	}

	public:
	SDL_Renderer * renderer;
	SDL_Texture * image;
	SDL_FRect pos;
	float maxY;
	SDL_FPoint speed;
	int hp;
	bool onGround;
	static float gravity;

};
float object::gravity = 0.24f;

class fountain : public object
{
	public:
	fountain(SDL_Renderer * screen) : object(screen)
	{
		running = true;
		lastSpawn = 0;
		delay = 0;
		flow = 10;
		pressure = 15;
		hp = 500;
	}

	void clear()
	{

		size_t len = objects.size();
		for(size_t i = 0; i < len; i ++)
		{
			delete objects[i];
			objects[i] = NULL;
		}
		objects.clear();
	}

	void update()
	{
		if(objects.size() > 10000)
		{
			// let's clean house
			std::vector <object*> temp;
			size_t len = objects.size();
			for(size_t i = 0; i < len; i ++)
			{
				objects[i]->update();
				if(objects[i]->hp > 0)
				{
					temp.push_back(objects[i]);
				}
				else
				{
					delete objects[i];
				}
			}
			objects.clear();
			objects = temp;
		}
		size_t tick = SDL_GetTicks();
		if(running && tick > lastSpawn + delay)
		{
			lastSpawn = tick;
			int count = flow;
			while(count > 0)
			{
				object * temp = new object(renderer);
				temp->setPos(pos.x, pos.y);
				temp->speed.x = 5.0f - (float((rand()%150)))/15.0f;
				temp->speed.y = -pressure - float(rand() % (150)/15);
				temp->hp = hp;
				temp->setImage(image);
				objects.push_back(temp);
				count --;
			}
		}
		size_t len = objects.size();
		for(size_t i = 0; i < len; i ++)
		{
			objects[i]->update();
		}
		SDL_Log("Total Objects: %ld", len);
	}

	void setParticleLife(int val)
	{
		hp = val;
	}

	void setFlow(int val)
	{
		flow = val;
	}
	void setPressure(int val)
	{
		pressure = val;
	}

	void setImage(SDL_Texture * src)
	{
		image = src;
	}

	void draw()
	{
		SDL_RenderTexture(renderer, image, NULL, &pos);
		size_t len = objects.size();
		for(size_t i = 0; i < len; i ++)
		{
			if(objects[i]->hp > 0)
			{
				objects[i]->draw();
			}
		}
	}

	public:
	bool running;
	int delay;
	int flow;
	int pressure;
	size_t lastSpawn;
	std::vector <object *> objects;
	int hp;
};

int main()
{
	SDL_Init(SDL_INIT_VIDEO);
	IMG_Init(IMG_INIT_PNG);
	SDL_Window * window = SDL_CreateWindow("Fountain", 1000, 1000, SDL_WINDOW_RESIZABLE);
	SDL_Renderer * screen = SDL_CreateRenderer(window, 0);
	SDL_SetRenderVSync(screen, 1);

	SDL_Texture * fire = IMG_LoadTexture(screen, imagePath.c_str());
	if(!fire)
	{
		SDL_Log("Could not load fire.png");
	}
	fountain splash(screen);
	splash.setImage(fire);
	splash.setPos(500, 1090);
	splash.setFlow(10);
	splash.setPressure(13);
	splash.setParticleLife(180);

	bool run = true;
	while(run)
	{
		SDL_Event ev;
		while(SDL_PollEvent(&ev))
		{
			switch(ev.type)
			{
				case SDL_EVENT_QUIT:
					run = false;
					break;
			}
		}
		splash.update();
//		splash.setPos(940.0f + 940.0f * float(sin(SDL_GetTicks()/5000.0f)), 1090);
		SDL_SetRenderDrawColor(screen, 20, 20, 200, 255);
		SDL_RenderClear(screen);
		splash.draw();
		SDL_RenderPresent(screen);
	}

	splash.clear();
	SDL_DestroyTexture(fire);
	IMG_Quit;
	SDL_Quit();
}
