COCOOSDIR = path/to/cocoOS
SRCDIR = ./src
OUTDIR = ./out
INC = -I./inc -I$(COCOOSDIR)/inc

OBJS = $(patsubst $(SRCDIR)/%.c,$(OUTDIR)/%.o,$(wildcard $(SRCDIR)/*.c))
COCOOSOBJS := $(wildcard $(COCOOSDIR)/obj/*.o)

all: outdir | application cocoOS
	gcc $(INC) $(OBJS) $(COCOOSOBJS) -lpthread -o appl

application: $(OBJS)

$(OUTDIR)/%.o : $(SRCDIR)/%.c
	gcc -c -Wall $(INC) $< -o $@

cocoOS:
	cd $(COCOOSDIR) && make
	
clean:
	rm ./out/*
	rm appl
	
.PHONY: outdir

outdir: 
	@mkdir -p out