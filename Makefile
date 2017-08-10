all:
	cd genscat; ./Set_Makeoptions; make
	cd rfscat; make

clean:
	cd rfscat; make clean
	cd genscat; make clean
