all: exe/canta-dbg-fr.exe exe/canta-dbg-en.exe exe/canta-fr.exe exe/canta-en.exe

obj_dbg/win_resources.o:
	windres -i src/graphics/canta.res -o obj_dbg/win_resources.o

CMD=gnatmake -d -Psrc/canta.gpr -XLIBRARY_TYPE=static

exe/canta-dbg-fr.exe: obj_dbg/win_resources.o
	$(CMD) -Xlangue=fr -Xbuild=Full_debug

exe/canta-dbg-en.exe: obj_dbg/win_resources.o
	$(CMD) -Xlangue=en -Xbuild=Full_debug

exe/canta-fr.exe: obj_dbg/win_resources.o 
	$(CMD) -Xlangue=fr -Xbuild=Full_prod

exe/canta-en.exe: obj_dbg/win_resources.o
	$(CMD) -Xlangue=en -Xbuild=Full_prod
