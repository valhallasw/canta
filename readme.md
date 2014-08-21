Canta is a tool to help you sing in tune. It can play a MIDI song and it displays the notes on the screen as they are played. It analyses the voice of the user and displays it on the same screen. For more information, please see http://www.singintune.org/

Canta was written by Christophe Chaumet and is available under the GPLv3 license.

## building instructions
Canta is written in Ada, so you will need to install an Ada compiler. Download GNAT 2014 and win32ada from the [adacore website][1].

If you have make installed, open a shell in the canta directory and run
```
make
```

This will create four executables in exe/: French and English, and a production and debug version of both.


If you do *not* have make installed, try:
```
windres -i src/graphics/canta.res -o obj_dbg/win_resources.o
gnatmake -d -Psrc/canta.gpr -XLIBRARY_TYPE=static -Xlangue=en -Xbuild=Full_prod
```

to build exe/canta-en.exe.

## development
After running the makefile once (or running windres), you can use GPS to develop. Double-click src/canta.gps and GPS will open the project. In GPS, you can then use the 'compile project' buttons.



[1]: http://libre.adacore.com/
