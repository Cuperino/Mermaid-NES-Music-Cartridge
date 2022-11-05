Mermaid NES Music Cartridge
=========

Mermaid NES Music Cartridge is a music cartridge for the NES/Famicom that plays a cover of the song Mermaid by Tatsuro Yamashita using the console's [Ricoh 2A03](https://en.wikipedia.org/wiki/Ricoh_2A03) sound chip. The song plays while a recreation for the Famicom of the "Pocket Music" music album cover is displayed.

Mermaid NES Music Cartridge was created as an aniversary present to my girlfriend. Happy aniversary Abby!

![Screenshot of Pocket Music's album cover remake for the NES/Famicom](https://github.com/Cuperino/Mermaid-NES-Music-Cartridge/raw/main/screenshots/2.jpg)

The music and its instrument voices were created using [Famitracker](http://famitracker.com/). An extra channel from the Famicom Disk System expansion pack is used for padding and chorus; this channel is optional and not used in the NES ROM. Artwork was created using [GNU Image Manipulation Program](https://www.gimp.org/), [Tiled](https://www.mapeditor.org/), and [NEXXT](https://frankengraphics.itch.io/nexxt). Software is written in 6502 Assembly for the [cc65 compiler toolchain](https://cc65.github.io/).

The ROM runs in NTSC. It has been tested to work on MiSTer FPGA hardware using the [NES_MiSTer core](https://github.com/MiSTer-devel/NES_MiSTer), and on Linux builds of the [FCEUX](https://fceux.com/web/home.html) and [Nestopia](https://nestopia.sourceforge.net/) emmulators.

To compile the ROM yourself, you must first have cc65 installed on your system. The `build.sh` script provides a convenient way to build Mermaid NES Music Cartridge on a Linux system. You may refer to the contents of said file to adapt the build commands to other systems.

# Software License
Mermaid NES Music Cartridge is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.

Mermaid NES Music Cartridge is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Foobar. If not, see <https://www.gnu.org/licenses/>.

The software is distributed under the GNU General Public License in compliance with the license requirements to use the [NSF Driver](http://famitracker.com/downloads.php).

# Music and Artwork Copyright Disclaimers

Music contained is a chiptune cover of the song "Mermaid" by Tatsuro Yamashita, copyright 1986, distributed by Sony Music Publishing (Japan) Inc.

Artwork is a manual recreation for the NES of the cover art for the music album "Pocket Music" by Tatsuro Yamashita, copyright 1986, distributed by Sony Music Publishing (Japan) Inc.

This cover of Mermaid and the Pocket Music's cover artwork recreations are a fan work, done by Javier O. Cordero Pérez in the year 2022.

Remaining works are Copyright © 2022 Javier O. Cordero Pérez
