# Bullfrog Ada Library
## Introduction
The Bullfrog library is an collection of various Ada packages, some useful and some just for play.  It includes packages that work with access types, containers, tasking, and low level memory access.

## License
This project is licensed under the **Mozilla Public License (version 2.0)**.  A local copy of the license is available in this repository.  It can also be found at https://mozilla.org/MPL/2.0/

## Platforms
The Bullfrog library currently builds and runs under mingw64 x86_64 FSF GNAT 13.2.  I have not tested on linux or with other compilers than GNAT.  The following platforms have been tested:

__Windows 11__
* FSF GNAT 13.2.0 (GPLv3 with Runtime Exception)
* FSF GNAT 14.1.3 (GPLv3 with Runtime Exception)

__Linux__
* Untested

### Building
* **GPRBuild** - Simply type `gprbuild` at the root directory
    * `gprbuild -XBUILD=development` is the default build type and uses compiler switches suitable for development.
    * `gprbuild -XBUILD=release` builds the library with suitable production switches and optimization.
* **Alire** - Simply type `alr build` at the root directory
    * `alr build --development` is the default build type and uses compiler switches suitable for development.
    * `alr build --release` builds the library with suitable production switches and optimization.
    * `alr build --validation` is not yet supported and currently defaults to the same configuration as `alr --development`.

## Components
### Smart_Access Types
In the **Bullfrog.Access_Types.Smart_Access** and **Bullfrog.Access_Types.Advanced_Smart_Access** packages, the Bullfrog library provides a variety of types designed to make memory management as automatic as possible.  These are intended to be "building block" types and not client facing types.  In a basic sense, the 3 primary types are:

* **Shared_Access** - Manages an access type so that it can be shared among various objects.  A Shared_Access object is copiable but it does a shallow copy (so copies the internal access variable, not the object itself).  This type, when instantiated from the Advanced_Smart_Access generic package, can be used with Incomplete formal types in generics to make self referencial data structures, but be aware of circular dependancies if you do so.

* **Weak_Access** - Primarily used to break circular dependencies in self referencial types that use Shared_Access objects.  Additionally, they can be used to emulate "handle" type designs since they do not allow direct access to the object.  A Weak_Access object must be promoted to a Shared_Access object in order to access the underlying resource.

* **Unique_Access** - Provides single ownership of a resource.  This is most useful for data structures where you need to allocate an object and automatically deallocate the object when the data structure goes out of scope.  While Unique_Access objects cannot be copied or shared, you can move their contents to a different Unique_Access object or even a Shared_Access object.  The previous owner gives up ownership in these situations.

Smart_Access types don't have atomic reference counting by default.  This can be turned on when instantiating the generic if using them in a multithreaded context.  Even when atomic reference counts are enabled, the Smart_Access types are not themselves task safe, nor are the resources task safe.  Protected objects or other primatives must be used to achieve task safety in those two contexts.

The main differences between the Smart_Access and Advanced_Smart_Access packages is that the Smart_Access package is intended to be used with most types while the various Advanced_Smart_Access packages are intended to be used with incomplete formal types and/or types that need custom storage pools.  In 99% of applications, the Smart_Access will probably be the package of choice.  Only use the Advanced_Smart_Access package if:

* You need a self referencing data structure
* You need to provided the access type used (say to provide a storage pool or another library's type)
<br><br>
<table border=6px>
   <tr><th>Package</th><th>Special Types Supported</th></tr>
   <tr>
      <td>Smart_Access</td>
      <td>Indefinite<br>
          Limited
      </td>
   </tr>
   <tr>
      <td>Advanced_Smart_Access</td>
      <td>Indefinite<br>
          Limited<br>
          Incomplete<br>
          Types with custom storage pools
      </td>
   </tr>
</table><br>

***

### Circular_Buffer Types
In the **Bullfrog.Containers.Circular_Buffers** package, the library provdes a 1 consumer to 1 producer lock free circular buffer.  In the majority of cases where two threads share a buffer, a protected object wrapping the buffer is the correct method.  however, there are instances where either you can't use protected objects (some runtimes restrict this) or you find that a lock free alternative is better.  This package provides this functionality as long as there is only one consumer and one producer.  If the program is single threaded, use a regular non synchronized buffer.  If the program has either more than one consumer or more than one producer, look for other alternatives such as a protected buffer.

***

### Sync_Wrapper types
In the **Bullfrog.Synchronization.Sync_Wrappers** package, the Bullfrog library provides a wrapper type that can be used to quickly add synchronization to another more complex type that doesn't provide it by default. To use the package, simply instantiate a generic with the type that you want and create a variable with the type from the generic.  You then can call variable_name.lock.some_operation_or_variable and it will autmoatically add synchronization around the call.  **There are some known bugs in GNAT when doing this in a declarative region though, so keep an eye for any if you use this package.  They will manifest as deadlocks (Essentially GNAT doesn't always call Finalize on objects in certain situations).**

***

### Endianess Functionality
In the **Bullfrog.Endianess** package, the library provides a means to detect what type of byte order your system uses.  The Ada language does not provide this standard.  It only covers bit order (which is not the same and rarely an issue for modern processors).  It defines a type and a constant (elaborated at runtime) to indicate what byte order you system currently uses.  If the system is not Big_Endian or Little_Endian, it is defaulted to Middle_Endian as a catch all.

***

### Modular_To_Array_Conversions operations
In the **Bullfrog.Modular_To_Array_Conversions** package, the library provides may operations designed to convert from the multibyte Unsigned_x types found in the Interfaces package of the Ada stanadard library to arrays of Unsigned_8 values.  This is useful in both interfacing to hardware registers and also creating/parsing message interfaces.  The package provides four different contexts for the conversions:

* **Native_Arrays** - Converts native modular types to/from arrays ordered in the native byte order.  These are essentially just Unchecked_Conversions.

* **Byte_Swapped_Arrays** - Converts native modular types to/from arrays ordered in the reverse order of the native byte order.  This is useful for converting from Little_Endian to Big_Endian and vice versa.

* **Big_Endian_Arrays** - Converts native modular types to/from arrays ordered in Big_Endian byte order (Element 1 is the MSB).

* **Little_Endian_Arrays** - Converts native modular types to/from arrays orded in Little_Endian byte order (Element 1 is the LSB).

***

### Reference_Counts Types
In the **Bullfrog.Access_Types.Reference_Counts** package, the library provides a utility type that provides "reference counting" functionality with the option to either include or exclude atomic operations as the mechanism.  

***

### Mutexes Types
In the **Bullfrog.Synchronization.Mutexes** package, the library provides both a basic mutex type and a recursive mutex type for utility operations in other packages:

* **Basic_Mutex** - This is the most basic type of mutex.  Locking it more than once in the same thread will cause a deadlock.

* **Recursive_Mutex** - This type of mutex can determine which thread is the actual owner of the mutex currently and thus can allow multiple locks within the same thread.  The same number of unlocks must be called, however, to prevent deadlocks.

***

### Test and Debug Folders
These folders hold various tests and debug packages used by the tests to verify functionality.  They aren't written to the same coding standard as the library (this will be fixed one day).  They might also be useful for finding examples.  There is a separate GNAT project file for compiling the tests.


# Legal Stuff
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