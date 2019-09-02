# Table of Contents
1. [Emacs Commands](#emacs-commands)
2. [Emacs - Elpy](#emacs---elpy)
3. [Rest Client](#rest-client)
4. [UML diagrams](#uml-diagrams)
5. [Treemacs](#treemacs)


# Emacs Commands

## Quick guide about keyboard keys

| Key binding | Description                       |
|-------------|-----------------------------------|
| `C-x`       | *Control with x key*              |
| `M-x`       | *Option/alt with x key*           |
| `s-x`       | *Command with x key*              |
| `M-S-x`     | *Option/alt with shift and x key* |

## Basic

| Key binding      | Description                            |
|------------------|----------------------------------------|
| `C-x C-c`        | Quit Emacs                             |
| `C-c p p`        | Switch project                         |
| `M-.`            | Go to definition                       |
| `M-,`            | Go back                                |
| `Alt-w` or `s-c` | Copy                                   |
| `C-w` or `s-x`   | Cut                                    |
| `C-y` or `s-v`   | Paste                                  |
| `C-z`            | Undo                                   |
| `C-S-z`          | Redo                                   |
| `C-g`            | **Esc**                                |
| `C-c d`          | Duplicate line                         |
| `C-k`            | Kill/delete cursor to end of this line |
| `M-x`            | **Show all commands**                  |
| `C-x C-+`        | Zoom IN in current buffer              |
| `C-x C--`        | Zoom OUT in current buffer             |
| `C-x-1`          | One window                             |
| `C-x-2`          | Split window horizontally              |
| `C-x-3`          | Split window vertically                |
| `C-x-o`          | Switch between windows                 |
| `C-x C-s`        | **Save open buffers**                  |
| `M-;`            | Comment line                           |
| `M-S-up`         | Move line up                           |
| `M-S-down`       | Move line down                         |

## Python

| Key binding | Description                                                                      |
|-------------|----------------------------------------------------------------------------------|
| `C-c u`     | *LSP ui:* quick overview for classes, functions, vars and models of current file |


## Search
| Key binding | Description                                                                                                                |
|-------------|----------------------------------------------------------------------------------------------------------------------------|
| `C-c p s r` | **Find occurrences in project**   `-g*test*` search in files with test name , `-tpy` search in files with `py` extension |
| `C-s`       | Find in current file                                                                                                       |
| `C-c p h`   | Find files in project                                                                                                      |
| `C-x C-d`   | Project git status                                                                                                         |
| `C-x C-f`   | Open current file directory                                                                                                |
| `C-c p d`   | Display a list of all directories in the project                                                                           |
| `C-c p f`   | Display a list of all files in the project

## Buffers
| Key binding       | Description               |
|-------------------|---------------------------|
| `C-x b`           | View buffers              |
| `C-x k`           | Kill current buffer       |
| `C-x left/right`  | Move to other buffer      |
| `C-x i`           | Insert a file into buffer |

## Magit
| Key binding | Description                     |
|-------------|---------------------------------|
| `C-x s`     | Open buffer to see magit status |
| `C-x g u`   | Git pull                        |
| `C-x g p`   | Git push                        |
| `C-x g c`   | Git commit                      |
| `C-x g s`   | Git status                      |
| `C-x g b`   | Git blame                       |

## Move between lines
Add SHIFT if you want to select
* Char
	* `C-b` Move left char
	* `C-f` Move right char
* Word
	* `Option-b` Move left word
	* `Option-f` Move right word
	* `Option-left/right` Move left/right word
* Line
	* `C-a` Go to begin of the line
	* `C-e` Go to end of the line
	* `C-p` Go to previous line
	* `C-n` Go to next line
* File
	* `Fn-left` Go to begin of file
	* `Fn-right` Go to end of file


# Emacs - Elpy
	1. M-x elpy-config
	2. Install the packages that are not installed yet
	3. M-x elpy-rpc-restart
	4. Elpy commands should work, for example go to definition

* To add a new project in elpy:
	1. Clone project from github, ex conformity
	2. Run python3 -m pip install -e ./conformity
	3. M-x elpy-rpc-restart
	4. Now go to definition should works

# Rest client
- Install `restclient` with `M-x package-install`
- Create a new buffer and save it whenever you want, example `./emacs.d/rest/rest-client`.
- Paste this in the beginning of the buffer:

```
# -*- restclient -*-
#
```
Now you can start with your REST requests!. For example:

```
# -*- restclient -*-
#
# Gets  all Github APIs, formats JSON, shows response status and headers underneath.
# Also sends a User-Agent header, because the Github API requires this.
#
GET https://api.github.com
User-Agent: Emacs Restclient
```
* `C-c C-c` runs the query
* Remember to use `#` between queries
More info: https://github.com/pashky/restclient.el

# UML diagrams
To write UML diagrams using `plantuml` with `org-mode`:

- Make sure you have installed `org-mode` package
- Verify that `org-mode` package version has `plantuml` language in `.emacs.d/elisp/org-mode-config.el`:

```
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote (...
         (plantuml . t)
         ...)))
```
- Download `plantuml.jar` (http://plantuml.sourceforge.net/download.html) into your computer. By default `org-mode` considers file in `~/Dropbox/plantuml.jar`. If you want to can change this e.g:

```
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml.jar"))
```
Or you can use

```
M-x customize-variable<RET>
plantuml-jar-path<RET>
```

- Update these changes `M-x eval-buffer`
- Create a new file with `.org` extension and you're ready to start with your *UML diagrams in Emacs*:

## Sequence diagrams

**Example**


```
#+begin_src plantuml :file sequence.png
  Alice -> Bob: synchronous call
  Alice ->> Bob: asynchronous call
#+end_src
```
- Press `C-c C-c` and you should have a **Sequence diagram** in `sequence.png`

To know more about `plantuml sequence diagrams` syntax: http://plantuml.com/sequence-diagram

## Class diagrams

Plantuml uses `Graphviz` to draw class diagrams, so make sure to have _Graphviz_ installed.

**Example**

```
#+begin_src plantuml :file class.png

class Dummy {
  String data
  void methods()
}

class Flight {
   flightNumber : Integer
   departureTime : Date
}
Dummy -- Flight
#+end_src

```
- Press `C-c C-c` and you should have a **Class diagram** in `class.png`

To know more about `plantuml class diagrams` syntax: http://plantuml.com/class-diagram

# Treemacs
If you want to have your emacs with file tree, like this:
![Treemacs example](https://raw.githubusercontent.com/Alexander-Miller/treemacs/master/screenshots/screenshot.png)
- Init `treemacs` `C-x t t`
You can press `?` inside `treemacs` buffer and it will show a list of commands.
- `C-c C-p p` to add new projects with projectile
- `n/p` to navigate between projects
- `q` to close `treemacs`
- `TAB/RET` to open project
More info: https://github.com/Alexander-Miller/treemacs
