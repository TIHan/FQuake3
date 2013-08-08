# FQuake3
_F\# Implementation of id Software's Quake III Arena._

Supported Platforms:

* __Windows 32-bit__

##Notes
The current source has a long way to go before everything is implemented in F#, but you should be able to build and run what is there; however, that may vary until a release. Use at your own risk.

Any code related to QVMs have been entirely removed. LCC and the vm_* console variables are gone. So, that means you have to have compiled x86 dlls. In the future, this will be replaced by IL coded dlls. Research will be needed for security against cgame, game, ui assemblies to prevent malicious mod'ers. [This](https://subterraneangames.com/threads/requested-namespaces-for-dungeoneer-scripting.2406/) is an example.

The following is the order of which implementations will be done first (rough):

1. Renderer
 * tr_main.c port to F#
 * Port bits that make use of anything in tr_main.c; eventually this will lead up to getting rid of tr_main.c entirely - this will mark a first version release.
 * Start creating thin wrappers around native GL and GLFW calls. This may or may not include the window itself.
 * Finish porting - new version release
2. Low-Level Networking
 * New version release when done.
3. Engine
 * Try to remove any OS specific C code and let F# take care of most of it as best as it can.
 * Find a sound library that is preferably not GPL. Might look at OpenAL 1.1 / OpenALsoft.
 * If all goes well, we can start trying to build on Linux and OSX. New release.
4. CGame / UI
 * Research and proof out security, as CGame/UI/Game should be mod-able. Figuring out what to do with FSharp.Core in this case may be difficult; might need to customize. We don't want mod'ers using a mailboxprocessor ;)
5. Game / Botlib
 * Game logic, including physics, rules, and bots.

Also, performance will vary between build to build, quite significantly. When re-writing functions to F#, marshaling and interoping between C and the Mono Runtime, and vice versa, can promote a lot of object copying of all different sizes and rates. Once more code gets ported to F#, this won't, in theory, be an issue.

Quake3 C functions ported to F#, will be made __pure__. No side effects. Cases where something needs to not be pure, try to handle them in computation expressions (like monads in Haskell); unless we are doing interoping that we know for sure does not have any side effects. There will be cases where we can't make a function pure at a specific point in time, so for documentation purposes, we will mark functions that __are pure__ with the __[Pure]__ attribute; and in theory go back to the functions that are not and fix them, but will try to avoid this as much as possible. ;)

All data types will be __immutable__. This also means we cannot use .NET's array type, as it is considered mutable in F# due to being ubiquitous.

## Build and Run Instructions

You will need a commercial copy of Quake III Arena, preferably with Team Arena as well.

####Windows 7 / 8
Download and install the following:

* Visual Studio 2012 ( _[Trial](http://www.microsoft.com/visualstudio/eng/products/visual-studio-premium-2012)_ ) (_[Express Edition](http://www.microsoft.com/visualstudio/eng/products/visual-studio-express-for-windows-desktop#product-express-desktop) not tested_)
* [F# compiler](http://go.microsoft.com/fwlink/?LinkID=261287) (if not installed)
* [Mono 3.0.10](http://origin-download.mono-project.com/archive/3.0.10/windows-installer/mono-3.0.10-gtksharp-2.12.11-win32-0.exe)

#####Setting Up Mono
1. Browse to where you installed Mono.
2. Then, copy Mono's root folder, e.g. _C:\Program Files (x86)\Mono-3.0.10_, to your repository's _\lib\_ folder.
3. Finally, rename your copied Mono folder to just "Mono". e.g. _FQuake3\lib\Mono_ - Note: git will not pick Mono up in commits.

#####Using Quake Content
1. In your repository's root folder, create a new folder called "build". e.g. _FQuake3/build/_
2. If you have a copy of Quake III Arena, go inside its root folder. You should see "baseq3" and "missionpack" (If you have Team Arena). Copy both of them, if you have both, to your newly created _/build/_ folder in your repository.

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

####Ubuntu
_Not Ready_

####OSX

_Not Ready_

## Why F#?
* Strongly Typed
* Functional-First
* Concise Code compared to other mainstream languages
* Easier Testability
* Concurrency
* Native Interoperability
* Visual Studio + Xamarin Studio IDEs supported
* Cross-Platform with Mono
* .NET Ecosystem
* Mature
* Fun