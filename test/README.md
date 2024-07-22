# <p style="text-align: center;">Bullfrog Ada Compiler Tests</p>

# 1. Description
These are tests designed to validate funcitonality of the more complted parts of the code.

<u><b>Command Line Options</u></b>
* directory_name - Optional parameter to specify what directory to parse ada files from.  If this is ommited, it defaults to parsing this projects own source files (both main and test).

## 1.1. Running directly
In the top level directory execute the following command:<br>
`./bin/main [directory_name]`

## 1.2. Running via [Alire](https://alire.ada.dev/)
In the top level directory execute the following command:<br>
`alr run [-a directory_name]`

# 2. Building
This project can be build using gnatmake, gprbuild, and [Alire](https://alire.ada.dev/).  Other build options may be possible but have not been tested.

<u><b>Build Platforms</b></u><br>
GNAT v14.1.3 on msys2 (Win11)
GNAT v13.2.0 on msys2 (Win11)

## 2.1. Building with gnatmake
In the top level directory execute the following command:<br>
`gnatmake -o bin/main.exe -D obj src/main.adb`

You can supply whatever compiler switches you desire.  Take a look at the top level GPR file for ideas or see the [gnatmake reference docs](https://gcc.gnu.org/onlinedocs/gnat_ugn/Switches-for-gnatmake.html) for all available options.

Older compilers may need to use the `-gnat2022` switch to compile any new language features.  It is recommended that assertions are turned on via the `-gnata` switch.

## 2.2. Building with gprbuild
In the top level directory simply execute the `gprbuild` command (no arguments) for a development build or `gprbuild -XBUILD=release` for a release build.

## 2.3. Building with Alire
In the top level directory simply execute the `alr build` command (no additional arguments) for a development build or `alr build --release` for a release build.  There is no `validation` profile available at this time.  It defaults to `development` if used.

<u>NOTE 1:</u>  The Alire build process uses a separate GPR file in the alire subdirectory.  However, this GPR file simply wraps the GPR file in the top level directory.  To change any build settings, modify the GPR file in the top level directory.

<u>NOTE 2:</u>  If you do not wish to use Alire at all, you can safely delete the `alire.toml` file and the `alire` subdirectory (and its contents).

## 2.4. Dependencies
This project depends on a few system tools:
* `int system(const char *)` - C function found in the system library.  Should not print to stdout (stderr is ok).
* `diff` - Command used to calculate file differences.  It should support the switches `-iw` to ignore capitalization and whitespace (other than the newline character).

# 3. License
This project is licensed under the Mozilla Public License (version 2.0).  A local copy of the license is available in this repository.  It can also be found at https://mozilla.org/MPL/2.0/

# 4. Legal Stuff
Warranty and Liablity sections for this project (pulled from the license document):
```
************************************************************************
*                                                                      *
*  6. Disclaimer of Warranty                                           *
*  -------------------------                                           *
*                                                                      *
*  Covered Software is provided under this License on an "as is"       *
*  basis, without warranty of any kind, either expressed, implied, or  *
*  statutory, including, without limitation, warranties that the       *
*  Covered Software is free of defects, merchantable, fit for a        *
*  particular purpose or non-infringing. The entire risk as to the     *
*  quality and performance of the Covered Software is with You.        *
*  Should any Covered Software prove defective in any respect, You     *
*  (not any Contributor) assume the cost of any necessary servicing,   *
*  repair, or correction. This disclaimer of warranty constitutes an   *
*  essential part of this License. No use of any Covered Software is   *
*  authorized under this License except under this disclaimer.         *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*  7. Limitation of Liability                                          *
*  --------------------------                                          *
*                                                                      *
*  Under no circumstances and under no legal theory, whether tort      *
*  (including negligence), contract, or otherwise, shall any           *
*  Contributor, or anyone who distributes Covered Software as          *
*  permitted above, be liable to You for any direct, indirect,         *
*  special, incidental, or consequential damages of any character      *
*  including, without limitation, damages for lost profits, loss of    *
*  goodwill, work stoppage, computer failure or malfunction, or any    *
*  and all other commercial damages or losses, even if such party      *
*  shall have been informed of the possibility of such damages. This   *
*  limitation of liability shall not apply to liability for death or   *
*  personal injury resulting from such party's negligence to the       *
*  extent applicable law prohibits such limitation. Some               *
*  jurisdictions do not allow the exclusion or limitation of           *
*  incidental or consequential damages, so this exclusion and          *
*  limitation may not apply to You.                                    *
*                                                                      *
************************************************************************