CC			= gcc
CFLAGS		= -Wall -std=c99 -Werror -arch i386 `pkg-config --cflags mono-2` `pkg-config --cflags glib-2.0`
SOURCES		= $(wildcard *.c)
OBJECTS		= $(SOURCES:%.c=%.o)
LIBS		= `pkg-config --libs mono-2` `pkg-config --libs glib-2.0`
INCLUDES	= -I.
OUTPUT		= libM.a

release: CFLAGS += -O2 -DNDEBUG
release: $(SOURCES) $(OUTPUT)

debug: CFLAGS += -DDEBUG -g3
debug: $(SOURCES) $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	ar rcs $(OUTPUT) $(OBJECTS)

clean:
	rm -f $(OBJECTS) $(OUTPUT)

.c.o:
	$(CC) $(INCLUDES) -c -o $@ $< $(CFLAGS)
