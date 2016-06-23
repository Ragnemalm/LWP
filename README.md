# Lightweight IDE - Lightweight on your mind

## Why do we need another IDE?

Some people say they like Xcode. Well, I do not. It is a carelessly made design, very confusing and badly planned, which tries to do everything, and the result is that it doesn't do it well. There are symbols and options everywhere to the extent that you never find what you need. In particular, making an external compiler (like a Pascal compiler) work with it is a nightmare. Several people have worked on the problem, and although they manage to build scripts that can build applications, the system remains brittle and often fails.

Case in point: If you download a demo from Apple, you can spend a lot of time fiddling with project settings before you can build. With Lightweight IDE, it generally works right out of the box, maybe change some setting like adding a framework, and just compile and run!

This can be argued, but I conclude that another path would be valuable, if nothing else for getting a simpler, more focused and less confusing solution.

## What is Lightweight IDE

Lightweight IDE (LWP for short) is an easy-to-use application with the following major functions:
+ An editor, multi-window, with syntax coloring and pop-up function/uses menus.
+ A front-end to both Free Pascal, GCC, Ada (GNAT) and Java, with error reporting coupled to the editor.
+ A builder of application bundles.
+ A front-end to GDB and LLDB (incomplete but working).

That is, it does everything important that Xcode does!

[Lightweight IDE by Ingemar Ragnemalm](http://www.ragnemalm.se/lightweight/)
