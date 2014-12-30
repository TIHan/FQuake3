# FQuake3
_Attempted F\# Implementation of id Software's Quake III Arena._

![logo](https://raw.github.com/TIHan/FQuake3/master/fquake3_logo.png)

Supported Platforms:

* __Windows 32-bit__

# Update!

FQuake3 has been put to rest in pursuit of something a little less extreme. :) It was most ambitious and a bit over my head about 99% of the time. However, I learned a ton: math, functional programming, game engine development, language interop. If it wasn't for this project in the first place, I wouldn't be where I am at and doing what I am doing. This means all the hard work did pay for something!

## Build and Run Instructions

You will need a commercial copy of Quake III Arena, preferably with Team Arena as well.

####Windows 7 / 8
Download and install the following:

* Visual Studio 2013 ( _[Trial](http://www.microsoft.com/visualstudio/eng/downloads#d-2013-editions)_ ) ( _[Express for Windows](http://www.microsoft.com/visualstudio/eng/downloads#d-2013-express) not tested_ )
* [Mono 3.2.3](http://download.mono-project.com/archive/3.2.3/windows-installer/mono-3.2.3-gtksharp-2.12.11-win32-0.exe)

#####Setting Up Mono
1. Browse to where you installed Mono.
2. Then, copy Mono's root folder, e.g. _C:\Program Files (x86)\Mono-3.0.10_, to your repository's _\lib\_ folder.
3. Finally, rename your copied Mono folder to just "Mono". e.g. _FQuake3\lib\Mono_ - Note: git will not pick Mono up in commits.

#####Using Quake Content
1. In your repository's root folder, create a new folder called "build". e.g. _FQuake3\build\_
2. If you have a copy of Quake III Arena, go inside its root folder. You should see "baseq3" and "missionpack" (If you have Team Arena). Copy both of them, if you have both, to your newly created _\build\_ folder in your repository.

#####Solution Configuration and Running

1. Open FQuake3.sln in Visual Studio 2012.
2. Looking at your solution in Solution Explorer, expand the "C" folder, and you will find a FQuake3 project.
3. Right click on the FQuake3 project, and select "Set as StartUp Project".
4. Right click again on the FQuake3 project, and select "Properties". You should now be in "FQuake3's Property Pages".
5. Next, in "FQuake3's Property Pages", choose your active Configuration in the top left to be "All Configurations".
6. Then, expand "Configuration Properties", and go to "Debugging".
7. Replace "Command Arguments" with: __--mono-lib "..\\lib\\Mono\\lib" --mono-etc "..\\lib\\Mono\\etc__"
8. Replace "Working Directory" with: __$(SolutionDir)build\__
9. Then, click Apply, then OK.
1. Choose "Debug" or "Release" for active build configuration, and then __build the solution__.
1. Finally, Start Debugging or Start without debugging.

Notes:

* Team Arena configuration builds are "Debug TA" and "Release TA". Make sure you have built one of the other configurations before building and running these.
* You can only Debug C code. F# code is not debug-able currently, however it is possible, but requires embedded mono configuration to set up a debug agent, and have a remote debugger, probably from Xamarin Studio to listen and use the F# source files. Also, the assembly .pdb debug files will need to be converted to .mdb using Mono's _pdb2mdb_.
