slider
=====

A slideshow software stack that is janky as hell, but also what I wanted to use.
A work in progress.

Why?
----

Regular slideshow software is decent, but doesn't necessarily do what I want. The following features are often supported by many tools, but rarely all at once:

- automatic handling of syntax highlighting in code snippets
- code snippets that can actually be loaded from files rather than wedged in a slide forever
- format for slideset spec that is easy to prototype and check into source control
- presenter notes
- friendly for screenshare mode on a single display
- working without network connectivity
- multiplatform (Linux, OSX, Windows)
- portable (could work without system install, and loadable from a USB drive)
- minimal presentation size at rest
- just automatically crop and center most images for backgrounds for me
- while you're at it do that for images I put in slides on the foreground too
- easy ways to make a basic "template" for style guidelines
- unicode support for things like emojis (everyone does it but I still need it)
- get out of the way

Also I wanted to try my hand at GUI programming, I've never done that before.

What does it look like?
-----------------------

All the content is put in a given directory where a single file with a `.set` extension is located.

```
=== defaults ===
// this section defines attributes that are defaults for all slides
text color: #ffffff
background: sloths.jpg
type: base
=== slides ===
// Now we start with actual slides. Each slide has two sections.
// This is the slide content for a 'base' slide:
title: This is one of the slides
subtitle: with a subtitle
---
// This is the presenter notes section
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam erat nisl,
rhoncus vel quam in, accumsan tincidunt leo. Vivamus condimentum congue
sagittis. Vestibulum eget orci placerat, suscipit ligula ut, ullamcorper dui.

===
title: Other slide
subtitle: Third subtitles
// specific attributes can be overridden
background: control.jpg
text color: #66FF66
---
Just say some things
- this is one
- this is another

===
title: Emojis!
subtitle: To the moon! 🌕
text color: #000000
---
Checking for Unicode support: 👩‍👩‍👦‍👦
This works hopefully

===
// for code slides, the 'source's extension specifies the highlight language
type: code
source: rebalance1.erl
source size: 32
title: Rebalance
---
This is part of the rebalancing of a treap

===
type: code
source: elixir.ex
source size: 32
title: Elixir
---
Elixir falls back to ruby as it has a similar syntax
and Wx doesn't have elixir

===
// image slides show just one big image
type: image
title: horizontal image
image: wind.jpg
---

```

The slideshow is started by calling the `show.sh` executable (see [Build](#build)) within the directory calling this.

Here are some of the rendered slides:

![](https://i.imgur.com/d8fOLUm.png)
![](https://i.imgur.com/Kszxes1.png)
![](https://i.imgur.com/UvyLJDw.png)


Stuff that still sucks
----------------------

- Haven't tested the build on Windows, which is sure to need a bunch of patching
- Nightmode on OSX clashes with the notes' panes default style
- The notes pane on linux includes the size of the title bar in its size so that sometimes truncates the last line of notes halfway through
- Haven't looked to make fonts portable yet so the portability part of this software is not fully there yet
- if you close one of the windows you gotta reboot the whole freaking thing to bring em back
- I picked "thousandths of screen height" for font-size so it scales with window size and it works but it's completely unintuitive
- font sizes turn out to look different across platforms and need fixing
- wouldn't be surprised if there were memory leaks in there
- the code is messy

Things I have not started figuring out (and may never do)
---------------------------------------------------------

- reloading of slide descriptions when editing them
- adding more languages to syntax highlight (currently: Erlang, Python, Ruby, C/C++, Elixir (via Ruby), Go (via C))
- proper preview mode with small versions of slides
- testing with a clicker for event registration
- using comments in code to load a subset of a file like in asciidoctor
- would be nice to automatically make text fit the boxes available in terms of size
- export to PDF
- automated tests of slide rendering
- themes for syntax highlighting
- slides with an interactive REPL in them
- slides with paragraphs of text
- credits slide that loads citations from files
- tables and charts

Things I can't be bothered with
-------------------------------

- Animations
- Embedded video/gifs

Build
-----

    $ rebar3 as prod release

And run with:

    $ /path/to/slider/_build/prod/rel/slider/show.sh

I have an issue or suggestion
-----------------------------

This is a toy project. Come have a chat.
