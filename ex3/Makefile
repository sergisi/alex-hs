##
# Exercici 2 - Pràctica 1
#
# @file
# @version 0.1
%.alex.hs : %.x
	alex $< -o $@

%.o : %.alex.hs
	ghc $< -o $@

all : Main.alex.hs Main.o

clean:
	rm -f *.o *.hi *.alex.hs *.happy.hs *.bin

# end
