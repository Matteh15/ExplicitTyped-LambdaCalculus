ifdef OS
	RM = del /Q
else
	RM = rm -f
endif

BASEDIR = ./app
MAIN = $(BASEDIR)/main.hs
ELANG = $(BASEDIR)/ELang.hs

build:
	ghc --make $(MAIN) $(ELANG)

run:
	./app/main.exe

clean:
	$(RM) .\app\*.exe .\app\*.o .\app\*.hi

.PHONY: clean