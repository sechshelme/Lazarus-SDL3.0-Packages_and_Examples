#include <SDL3/SDL.h>
#include <vector>
#include <cstdlib>

void fillColor(SDL_Renderer * renderer, SDL_Texture * texture, SDL_Color color)
{
	SDL_SetRenderTarget(renderer, texture);
	SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, 255);
	SDL_RenderClear(renderer);
	SDL_SetRenderTarget(renderer, NULL);
}

class Node
{
	public:
	Node(SDL_Renderer * screen)
	{
		renderer = screen;
		pos = {0, 0, 15, 15};
		image = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, pos.w, pos.h);
		fillColor(renderer, image, {100, 200, 100, 255});
		link = NULL;
		backLink = NULL;
	}

	bool isHit(SDL_FPoint &pt)
	{
		return SDL_PointInRectFloat(&pt, &pos);
	}
	void clear()
	{
		SDL_DestroyTexture(image);
		if(backLink->link == this)
		{
			backLink->unlink();
		}
	}
	void setPos(float x, float y)
	{
		pos.x = x;
		pos.y = y;
	}
	void unlink()
	{
		link = NULL;
		backLink = NULL;
	}
	void linkTo(Node * other)
	{
		link = other;
		other->backLink = this;
	}
	void draw()
	{
		SDL_RenderTexture(renderer, image, NULL, &pos);
		if(link)
		{
			SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
			SDL_RenderLine(renderer, pos.x + pos.w/2, pos.y + pos.h/2, link->pos.x + link->pos.w/2, link->pos.y + link->pos.h/2);
		}
	}

	public:
	SDL_Renderer * renderer;
	SDL_Texture * image;
	SDL_FRect pos;
	Node * link;
	Node * backLink;
};

std::vector <Node * > inputs;

class Attribute
{
	public:
	Attribute(SDL_Renderer * screen)
	{
		dragging = false;
		value = 0;
		renderer = screen;
		pos = {0, 0, 80, 50};
		image = NULL;
		image = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, pos.w, pos.h);
		fillColor(renderer, image, {200, 200, 120, 255});
		input = new Node(renderer);
		inputs.push_back(input);
		output = new Node(renderer);
		dragLink = NULL;
		setPos(rand() % 900, rand() % 900);
	}
	void clear()
	{
		SDL_DestroyTexture(image);
		image = NULL;
		delete input;
		delete output;
	}
	void setPos(float x, float y)
	{
		input->setPos(x - 5, y);
		output->setPos(x + pos.w - 10, y);
		pos.x = x;
		pos.y = y;
	}

	void handleEvent(SDL_Event * ev)
	{
		switch(ev->type)
		{
			case SDL_EVENT_MOUSE_MOTION:
				mouse = {ev->button.x, ev->button.y};
				if(dragLink)
				{
					dragLink->setPos(mouse.x - 8, mouse.y - 8);
				}
				else if(dragging)
				{
					setPos(mouse.x, mouse.y);
				}
				break;
			case SDL_EVENT_MOUSE_BUTTON_DOWN:
				if(output->isHit(mouse))
				{
					// Holding the output node.
					if(output->link)
					{
						output->link->unlink();
					}
					output->unlink();
					dragLink = new Node(renderer);
					dragLink->setPos(mouse.x, mouse.y);
					output->linkTo(dragLink);
				}
				else if(input->isHit(mouse))
				{
					input->backLink->unlink();
					input->backLink = NULL;
				}
				else if(SDL_PointInRectFloat(&mouse, &pos))
				{
					dragging = true;
				}
				break;
			case SDL_EVENT_MOUSE_BUTTON_UP:
				if(dragLink)
				{
					if(output->link)
					{
						output->unlink();
					}
					size_t len = inputs.size();
					for(size_t i = 0; i < len; i ++)
					{
						if(inputs[i]->isHit(mouse))
						{
							if(inputs[i]->backLink == NULL)
							{
								output->linkTo(inputs[i]);
							}
						}
					}
					dragLink->clear();
					delete dragLink;
					dragLink = NULL;
				}
				dragging = false;
				break;
		}
	}

	void draw()
	{
		if(image)
		{
			SDL_RenderTexture(renderer, image, NULL, &pos);
			input->draw();
			output->draw();
		}
	}

	public:
	SDL_Renderer * renderer;
	SDL_Texture * image;
	SDL_FRect pos;
	SDL_FPoint mouse;
	Node * input;
	Node * output;
	Node * dragLink;
	bool dragging;
	int value;
};


int main()
{
	SDL_Init(SDL_INIT_VIDEO);
	SDL_Window * win = SDL_CreateWindow("title", 1000, 1000, SDL_WINDOW_RESIZABLE);
	SDL_Renderer * screen = SDL_CreateRenderer(win, 0, SDL_RENDERER_PRESENTVSYNC);

	std::vector <Attribute *> Attributes;
	Attributes.push_back(new Attribute(screen));
	Attributes.push_back(new Attribute(screen));
	Attributes.push_back(new Attribute(screen));
	Attributes.push_back(new Attribute(screen));
	bool run = true;
	while(run)
	{
		SDL_Event ev;
		while(SDL_PollEvent(&ev))
		{
			size_t len = Attributes.size();
			for(size_t i = 0; i < len; i ++)
			{
				Attributes[i]->handleEvent(&ev);
			}
			switch(ev.type)
			{
				case SDL_EVENT_QUIT:
					run = false;
					break;
			}
		}
		SDL_SetRenderDrawColor(screen, 95, 95, 125, 255);
		SDL_RenderClear(screen);
		size_t len = Attributes.size();
		for(size_t i = 0; i < len; i ++)
		{
			Attributes[i]->draw();
		}
		SDL_RenderPresent(screen);
	}
	SDL_Quit();
}
