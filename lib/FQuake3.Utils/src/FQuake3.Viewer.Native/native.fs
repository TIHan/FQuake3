namespace FQuake3.Viewer.Native.GL

open Ferop.Code
open System.Runtime.InteropServices

#nowarn "9"

[<Struct>]
type Application =
    val Window : nativeint
    val GLContext : nativeint

[<Ferop>]
[<ClangFlagsOsx ("-DGL_GLEXT_PROTOTYPES")>]
[<ClangLibsOsx ("-framework Cocoa -framework OpenGL -framework IOKit -framework SDL2")>]
[<Include ("<stdio.h>")>]
[<Include ("<SDL2/SDL.h>")>]
[<Include ("<SDL2/SDL_opengl.h>")>]
module App =
    let init () : Application =
        C """
SDL_Init (SDL_INIT_VIDEO);

App_Application app;

app.Window = 
    SDL_CreateWindow(
        "Ferop.Sample",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        900, 900,
        SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

app.GLContext = SDL_GL_CreateContext ((SDL_Window*)app.Window);
return app;
        """

    let exit (app: Application) : int =
        C """
SDL_GL_DeleteContext (app.GLContext);
SDL_DestroyWindow (app.Window);
SDL_Quit ();
return 0;
        """

    let clear () : unit = C """glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);"""

    let depthTest () : unit =
        C """
glEnable (GL_DEPTH_TEST);
glDepthFunc (GL_LESS);
"""

    let draw (app: Application) : unit = C """SDL_GL_SwapWindow (app.Window);"""

    let shouldQuit () : bool =
        C """
SDL_Event e;
SDL_PollEvent (&e);
return e.type == SDL_QUIT;
        """

    let generateVao () : unit =
        C """
GLuint vao;
glGenVertexArrays (1, &vao);
glBindVertexArray (vao);
        """

    let generateVbo (size: int) (data: nativeint) : int =
        C """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ARRAY_BUFFER, vbo);

glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    let generateElementVbo (size: int) (data: nativeint) : int =
        C """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, vbo);

glBufferData (GL_ELEMENT_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    let drawData (vbo: int) (ebo: int) (count: int) : unit =
        C """
glEnableVertexAttribArray(0);
glBindBuffer(GL_ARRAY_BUFFER, vbo);
glVertexAttribPointer (
   0,                  // attribute 0. No particular reason for 0, but must match the layout in the shader.
   3,                  // size
   GL_FLOAT,           // type
   GL_FALSE,           // normalized?
   0,                  // stride
   (void*)0            // array buffer offset
);

glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
glDrawElements(GL_TRIANGLES, count, GL_UNSIGNED_INT, 0);
//glDrawArrays(GL_TRIANGLES, 0, count); 
glDisableVertexAttribArray(0);
        """

    let loadShaders (vertexSource: nativeint) (fragmentSource: nativeint) : uint32 =
        C """
GLint result = GL_FALSE;
int infoLogLength;

GLuint vertexShader = glCreateShader (GL_VERTEX_SHADER);
glShaderSource (vertexShader, 1, &vertexSource, NULL);    
glCompileShader (vertexShader);

// Check Vertex Shader
glGetShaderiv (vertexShader, GL_COMPILE_STATUS, &result);
glGetShaderiv (vertexShader, GL_INFO_LOG_LENGTH, &infoLogLength);
char vertexShaderErrorMessage[infoLogLength];
glGetShaderInfoLog(vertexShader, infoLogLength, NULL, &vertexShaderErrorMessage[0]);
fprintf(stdout, "%s\n", &vertexShaderErrorMessage[0]);

GLuint fragmentShader = glCreateShader (GL_FRAGMENT_SHADER);
glShaderSource (fragmentShader, 1, &fragmentSource, NULL);
glCompileShader (fragmentShader);

// Check Fragment Shader
glGetShaderiv (fragmentShader, GL_COMPILE_STATUS, &result);
glGetShaderiv (fragmentShader, GL_INFO_LOG_LENGTH, &infoLogLength);
char fragmentShaderErrorMessage[infoLogLength];
glGetShaderInfoLog(fragmentShader, infoLogLength, NULL, &fragmentShaderErrorMessage[0]);
fprintf(stdout, "%s\n", &fragmentShaderErrorMessage[0]);

/******************************************************/

GLuint shaderProgram = glCreateProgram ();
glAttachShader (shaderProgram, vertexShader);
glAttachShader (shaderProgram, fragmentShader);
glLinkProgram (shaderProgram);
glUseProgram (shaderProgram);

/******************************************************/
return shaderProgram;
    """

    let uniformMVP (programId: uint32) : uint32 =
        C """
// Get a handle for our "MVP" uniform
GLuint MatrixID = glGetUniformLocation(programId, "MVP");
return MatrixID;
        """

    let enableUniformMVP (mvpId: uint32) (mvp: nativeint) : unit =
        C """
// in the "MVP" uniform
glUniformMatrix4fv(mvpId, 1, GL_FALSE, mvp);
        """

    let generateTexture (width: int) (height: int) (data: nativeint) : uint32 =
        C """
// Create one OpenGL texture
GLuint textureID;
glGenTextures(1, &textureID);
 
// "Bind" the newly created texture : all future texture functions will modify this texture
glBindTexture(GL_TEXTURE_2D, textureID);
 
// Give the image to OpenGL
glTexImage2D(GL_TEXTURE_2D, 0,GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
 
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        """

    let drawUV (vbo: uint32) : unit =
        C """
// 2nd attribute buffer : colors
glEnableVertexAttribArray(1);
glBindBuffer(GL_ARRAY_BUFFER, vbo);
glVertexAttribPointer(
    1,                                // attribute. No particular reason for 1, but must match the layout in the shader.
    2,                                // size
    GL_FLOAT,                         // type
    GL_FALSE,                         // normalized?
    0,                                // stride
    (void*)0                          // array buffer offset
);
        """

    let bindTexture (textureId: uint32) (uniformTextureId: uint32) : unit =
        C """
// Bind our texture in Texture Unit 0
glActiveTexture(GL_TEXTURE0);
glBindTexture(GL_TEXTURE_2D, textureId);
// Set our "myTextureSampler" sampler to user Texture Unit 0
glUniform1i(uniformTextureId, 0);
        """

    let uniform (programId: uint32) (name: string) : uint32 =
        C """
return glGetUniformLocation(programId, name);
        """
