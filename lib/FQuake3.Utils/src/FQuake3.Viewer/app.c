#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>

#if defined(_WIN32)
#	define APP_IMPORT		__declspec(dllimport)
#	define APP_EXPORT		__declspec(dllexport)
#	define APP_DECL			__cdecl
#elif defined(__GNUC__)
#	define APP_EXPORT		__attribute__((visibility("default")))
#	define APP_IMPORT
#	define APP_DECL			__attribute__((cdecl))
#else
#	error Compiler not supported.
#endif

// App structure
typedef struct
{
	SDL_Window *window;
	SDL_GLContext *gl_context;
} app_t;

// Initialize app
APP_EXPORT app_t * APP_DECL
app_init (const char *title)
{
	static app_t app;

	SDL_Init (SDL_INIT_VIDEO);

	app.window = 
		SDL_CreateWindow(
			title,
			SDL_WINDOWPOS_UNDEFINED,
			SDL_WINDOWPOS_UNDEFINED,
			640, 480,
			SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

	SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
	SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
	
	app.gl_context = SDL_GL_CreateContext (app.window);

	return &app;
}

// Exit app
APP_EXPORT int APP_DECL
app_exit (app_t *app)
{
	SDL_GL_DeleteContext (app->gl_context);  
	SDL_DestroyWindow (app->window);
	SDL_Quit ();
	return 0;
}

APP_EXPORT void APP_DECL
app_clear_window (app_t *app)
{
	glClear (GL_COLOR_BUFFER_BIT);
	SDL_GL_SwapWindow (app->window);
}

// Loop
APP_EXPORT void APP_DECL
app_loop (app_t *app, void (*callback)(void))
{
	SDL_Event e;

	while (e.type != SDL_KEYDOWN && e.type != SDL_QUIT)
  	{
		SDL_PollEvent (&e);

		callback ();
	}
}

// Start app loop
/*
APP_EXPORT void APP_DECL
app_loop (app_t *app)
{
  	SDL_Event e;

	float vertices[] = {
	     0.0f,  0.5f, // Vertex 1 (X, Y)
	     0.5f, -0.5f, // Vertex 2 (X, Y)
	    -0.5f, -0.5f  // Vertex 3 (X, Y)
	};

	//******************************************************

  	GLuint vbo;
	glGenBuffers (1, &vbo);

	glBindBuffer (GL_ARRAY_BUFFER, vbo);

	glBufferData (GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);


	//******************************************************

	Uint8 vertexSource[4096] = { '\0' };
	Uint8 fragmentSource[4096] = { '\0' };

	load_file ("test.vertex", vertexSource, 4096);
	const GLchar *testVertex = strcat ((char *)vertexSource, "\0");

	load_file ("test.fragment", fragmentSource, 4096);
	const GLchar *testFragment = strcat ((char *)fragmentSource, "\0");

	GLuint vertexShader = glCreateShader (GL_VERTEX_SHADER);
	glShaderSource (vertexShader, 1, &testVertex, NULL);	
	glCompileShader (vertexShader);

	GLuint fragmentShader = glCreateShader (GL_FRAGMENT_SHADER);
	glShaderSource (fragmentShader, 1, &testFragment, NULL);
	glCompileShader (fragmentShader);

	//******************************************************

	GLuint shaderProgram = glCreateProgram ();
	glAttachShader (shaderProgram, vertexShader);
	glAttachShader (shaderProgram, fragmentShader);

	glBindFragDataLocation (shaderProgram, 0, "outColor");

	glLinkProgram (shaderProgram);

	glUseProgram (shaderProgram);

	//******************************************************

	GLuint vao;
	glGenVertexArrays (1, &vao);

	glBindVertexArray (vao);

	GLint posAttrib = glGetAttribLocation (shaderProgram, "position");

	glVertexAttribPointer (posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);

	glEnableVertexAttribArray (posAttrib);

	//******************************************************
  
  	while (e.type != SDL_KEYDOWN && e.type != SDL_QUIT) {
	    SDL_PollEvent (&e);

		glDrawArrays (GL_TRIANGLES, 0, 3);
	    
	    SDL_GL_SwapWindow (app->window);
	    SDL_Delay (10);
  	}
}
*/
