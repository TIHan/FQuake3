CC			= gcc
CFLAGS		= -Wall -std=c99 -arch i386 -DGL_GLEXT_PROTOTYPES
SOURCES		= $(wildcard *.c)
OBJECTS		= $(SOURCES:%.c=%.o)
LIBS		= -framework Cocoa -framework OpenGL -framework IOKit -framework SDL2
INCLUDES	= -I.
OUTPUT		= libFQ3Viewer.dylib
DYFLAGS		= -Wl -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0

release: CFLAGS += -O2
release: $(SOURCES) $(OUTPUT)

debug: CFLAGS += -DDEBUG -g3
debug: $(SOURCES) $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	$(CC) -arch i386 -dynamiclib $(DYFLAGS) $(LIBS) $(OBJECTS) -o $(OUTPUT)

clean:
	rm -f $(OBJECTS) $(OUTPUT)

.c.o:
	$(CC) $(INCLUDES) -c -o $@ $< $(CFLAGS)
