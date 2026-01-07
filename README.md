# Emacs Configuration

Configuración modular para Emacs 30.2+ con stack moderno de completion y LSP.

## Características

- **Arquitectura modular** - Configuración dividida en módulos lógicos
- **Completion moderno** - Vertico + Consult + Corfu (reemplaza Helm + Company)
- **LSP** - lsp-mode con pyright + ruff para Python, typescript-language-server para TS/React
- **Git** - Magit + diff-hl
- **Proyectos** - project.el built-in (reemplaza Projectile)
- **File tree** - Treemacs con nerd-icons
- **Tree-sitter** - Syntax highlighting mejorado para Python, TypeScript, TSX, JavaScript
- **nvm.el** - Detección automática de versión de Node por proyecto (.nvmrc)
- **fzf** - Fuzzy finder integrado con fdfind y ripgrep para búsqueda rápida
- **AI Copilot** - gptel (chat con Claude/Gemini) + GitHub Copilot (inline completion)
- **Dashboard** - Pantalla de inicio con proyectos recientes, archivos y bookmarks
- **fzf Preview** - Preview de archivos con bat y syntax highlighting (ctrl-/ toggle, ctrl-up/down scroll)
- **Mermaid Preview** - Preview de markdown con diagramas mermaid renderizados
- **Marksman LSP** - Autocompletado y navegación para Markdown

## Requisitos

- macOS (Apple Silicon) o Ubuntu/Linux
- Emacs 30.2+ compilado desde source
- Node.js (para mermaid-cli)
- uv (para herramientas Python)
- fzf (fuzzy finder)
- fd-find (buscador de archivos rápido)
- ripgrep (búsqueda de texto)
- bat (preview con syntax highlighting para fzf)
- marksman (LSP para Markdown)

## Instalación en macOS (Apple Silicon)

### 1. Instalar dependencias con Homebrew

```bash
brew install autoconf automake texinfo gnutls pkg-config jansson \
  librsvg libpng libjpeg-turbo giflib webp imagemagick \
  libgccjit gcc
```

> **Importante**: Se necesita `gcc` además de `libgccjit` para que native-comp funcione correctamente.

### 2. Clonar y compilar Emacs

```bash
# Clonar repositorio
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
git checkout emacs-30.2

# Configurar (sin tree-sitter - ver nota abajo)
./autogen.sh

export PKG_CONFIG_PATH="$(brew --prefix libgccjit)/lib/pkgconfig:$(brew --prefix librsvg)/lib/pkgconfig:$PKG_CONFIG_PATH"

./configure \
  --with-native-compilation \
  --with-ns \
  --with-json \
  --with-rsvg \
  --with-imagemagick

# Compilar
make -j$(sysctl -n hw.ncpu)
make install

# Copiar a Applications
sudo cp -r nextstep/Emacs.app /Applications/
```

### 2b. Habilitar Tree-sitter (opcional pero recomendado)

La versión de tree-sitter en Homebrew (0.26.x) usa ABI 15, incompatible con Emacs 30.2 (ABI 13-14).
Para habilitar tree-sitter, compilar la versión 0.24.6 desde source:

```bash
# Desinstalar versión de Homebrew si existe
brew uninstall tree-sitter 2>/dev/null || true

# Clonar y compilar versión compatible
git clone --depth 1 -b v0.24.6 https://github.com/tree-sitter/tree-sitter.git /tmp/tree-sitter
cd /tmp/tree-sitter
make -j$(sysctl -n hw.ncpu)
make install PREFIX=$HOME/.local

# Corregir pkgconfig (reemplazar /usr/local con tu home)
cat > ~/.local/lib/pkgconfig/tree-sitter.pc << EOF
prefix=$HOME/.local
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: tree-sitter
Description: An incremental parsing system for programming tools
URL: https://tree-sitter.github.io/tree-sitter/
Version: 0.24.6
Libs: -L\${libdir} -ltree-sitter
Cflags: -I\${includedir}
EOF

# Corregir install_name del dylib
install_name_tool -id "$HOME/.local/lib/libtree-sitter.0.24.dylib" ~/.local/lib/libtree-sitter.0.24.dylib

# Limpiar
rm -rf /tmp/tree-sitter
```

Luego recompilar Emacs con tree-sitter:

```bash
cd /path/to/emacs
make clean

TREE_SITTER_CFLAGS="-I$HOME/.local/include" \
TREE_SITTER_LIBS="-L$HOME/.local/lib -ltree-sitter" \
CPPFLAGS="-I$HOME/.local/include -I/opt/homebrew/opt/openssl@3/include" \
LDFLAGS="-L$HOME/.local/lib -L/opt/homebrew/opt/openssl@3/lib" \
./configure \
  --with-native-compilation \
  --with-ns \
  --with-tree-sitter \
  --with-rsvg \
  --with-imagemagick

make -j$(sysctl -n hw.ncpu)
make install
sudo cp -r nextstep/Emacs.app /Applications/
```

### 3. Configurar LIBRARY_PATH para native-comp

Agregar a `~/.zshrc` y `~/.zshenv`:

```bash
# Emacs native-comp (libgccjit)
export LIBRARY_PATH="/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current${LIBRARY_PATH:+:$LIBRARY_PATH}"
```

Para que Emacs.app (GUI) también tenga acceso:

```bash
source ~/.zshrc
launchctl setenv LIBRARY_PATH "$LIBRARY_PATH"
```

### 4. Crear comando `emacs` para terminal

```bash
mkdir -p ~/bin

cat > ~/bin/emacs << 'EOF'
#!/bin/bash
/Applications/Emacs.app/Contents/MacOS/Emacs "$@"
EOF

chmod +x ~/bin/emacs

# Symlink para emacsclient
ln -sf /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ~/bin/emacsclient
```

Asegurarse de tener `~/bin` en el PATH (en `~/.zshrc`):

```bash
export PATH="${HOME}/bin:$PATH"
```

---

## Instalación en Linux (Ubuntu/Debian)

### 1. Compilar Emacs desde source

```bash
# Dependencias de compilación
sudo apt install build-essential libgtk-3-dev libgnutls28-dev \
  libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev \
  libncurses-dev texinfo libjansson-dev libharfbuzz-dev \
  libgccjit-14-dev libsqlite3-dev libtree-sitter-dev librsvg2-dev

# Clonar repositorio
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
git checkout emacs-30.2

# Configurar y compilar
./autogen.sh
./configure --with-native-compilation --with-tree-sitter \
            --with-x-toolkit=gtk3 --with-rsvg --with-mailutils
make -j$(nproc)
sudo make install
```

### 2. Herramientas de búsqueda (fzf + fd + ripgrep)

```bash
# fzf - fuzzy finder
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# fd-find - buscador de archivos rápido (respeta .gitignore)
sudo apt install fd-find

# ripgrep - búsqueda de texto rápida
sudo apt install ripgrep
```

### 3. Clonar esta configuración

```bash
# Backup de configuración existente (si existe)
mv ~/.emacs.d ~/.emacs.d.backup

# Clonar
git clone <repo-url> ~/.emacs.d
```

### 5. Dependencias para Python

```bash
# Pyright - LSP server para tipos y autocompletado
uv tool install pyright

# Ruff - linter y formatter (global como fallback)
uv tool install ruff
```

> **Nota**: Cada proyecto Python debe tener su propio ruff:
> ```bash
> uv add ruff --dev
> ```
> La configuración detecta automáticamente el ruff del proyecto via `pet`.

### 6. Dependencias para Markdown

```bash
# Pandoc - export y preview
# macOS:
brew install pandoc bat marksman
# Linux:
# sudo apt install pandoc bat
# brew install marksman  # o descargar de https://github.com/artempyanykh/marksman/releases

# Grip - preview estilo GitHub
uv tool install grip

# Mermaid CLI - diagramas (requiere Node.js 20+)
npm install -g @mermaid-js/mermaid-cli
```

> **Nota**: `marksman` proporciona LSP para Markdown (go to definition, completado de links, etc.).

### 7. Dependencias para TypeScript/React

```bash
# TypeScript Language Server
npm install -g typescript-language-server typescript
```

> **Nota**: La configuración usa `nvm.el` para detectar automáticamente la versión de Node de cada proyecto via `.nvmrc`. Cada proyecto debe tener su propio `.nvmrc`:
> ```bash
> echo "22" > .nvmrc
> nvm use
> ```

### 8. Tree-sitter grammars

```bash
# IMPORTANTE: Usar versiones con ABI 14
# Las versiones nuevas usan ABI 15 que Emacs 30.2 no soporta

mkdir -p ~/.emacs.d/tree-sitter

# Python
git clone --depth 1 -b v0.23.2 \
  https://github.com/tree-sitter/tree-sitter-python.git /tmp/ts-python
cd /tmp/ts-python
# macOS:
cc -fPIC -shared -o libtree-sitter-python.dylib src/parser.c src/scanner.c -I src -O2
cp libtree-sitter-python.dylib ~/.emacs.d/tree-sitter/
# Linux:
# cc -fPIC -shared -o libtree-sitter-python.so src/parser.c src/scanner.c -I src -O2
# cp libtree-sitter-python.so ~/.emacs.d/tree-sitter/

# TypeScript y TSX
git clone --depth 1 -b v0.20.3 \
  https://github.com/tree-sitter/tree-sitter-typescript.git /tmp/ts-typescript
cd /tmp/ts-typescript/typescript
# macOS:
cc -fPIC -shared -o libtree-sitter-typescript.dylib src/parser.c src/scanner.c -I src -O2
cp libtree-sitter-typescript.dylib ~/.emacs.d/tree-sitter/
# Linux:
# cc -fPIC -shared -o libtree-sitter-typescript.so src/parser.c src/scanner.c -I src -O2
# cp libtree-sitter-typescript.so ~/.emacs.d/tree-sitter/

cd /tmp/ts-typescript/tsx
# macOS:
cc -fPIC -shared -o libtree-sitter-tsx.dylib src/parser.c src/scanner.c -I src -O2
cp libtree-sitter-tsx.dylib ~/.emacs.d/tree-sitter/
# Linux:
# cc -fPIC -shared -o libtree-sitter-tsx.so src/parser.c src/scanner.c -I src -O2
# cp libtree-sitter-tsx.so ~/.emacs.d/tree-sitter/

# JavaScript
git clone --depth 1 -b v0.21.2 \
  https://github.com/tree-sitter/tree-sitter-javascript.git /tmp/ts-javascript
cd /tmp/ts-javascript
# macOS:
cc -fPIC -shared -o libtree-sitter-javascript.dylib src/parser.c src/scanner.c -I src -O2
cp libtree-sitter-javascript.dylib ~/.emacs.d/tree-sitter/
# Linux:
# cc -fPIC -shared -o libtree-sitter-javascript.so src/parser.c src/scanner.c -I src -O2
# cp libtree-sitter-javascript.so ~/.emacs.d/tree-sitter/

# JSON
git clone --depth 1 -b v0.20.2 \
  https://github.com/tree-sitter/tree-sitter-json.git /tmp/ts-json
cd /tmp/ts-json
# macOS:
cc -fPIC -shared -o libtree-sitter-json.dylib src/parser.c -I src -O2
cp libtree-sitter-json.dylib ~/.emacs.d/tree-sitter/
# Linux:
# cc -fPIC -shared -o libtree-sitter-json.so src/parser.c -I src -O2
# cp libtree-sitter-json.so ~/.emacs.d/tree-sitter/

# Limpiar
rm -rf /tmp/ts-python /tmp/ts-typescript /tmp/ts-javascript /tmp/ts-json
```

### 9. Instalar fuentes (dentro de Emacs)

```
M-x nerd-icons-install-fonts RET
M-x all-the-icons-install-fonts RET
```

### 10. AI Copilot (gptel + GitHub Copilot)

```bash
# Variables de entorno para gptel (agregar a ~/.zshrc)
export CLAUDE_API_KEY="tu-api-key-de-anthropic"
export GEMINI_API_KEY="tu-api-key-de-google"  # opcional
```

Dentro de Emacs, instalar el servidor de Copilot y autenticarse:

```
M-x copilot-install-server RET    ; Instala @github/copilot-language-server
M-x copilot-login RET             ; Autentica con tu cuenta de GitHub
```

> **Nota**: gptel usa Claude Opus como modelo por defecto. GitHub Copilot requiere una cuenta (tiene tier gratis desde 2025).

### 11. Iniciar Emacs

```bash
emacs &
```

Los paquetes se instalarán automáticamente en el primer inicio.

## Estructura de Archivos

```
~/.emacs.d/
├── early-init.el              # Optimizaciones pre-UI
├── init.el                    # Bootstrap y carga de módulos
├── modules/
│   ├── core/
│   │   ├── core-packages.el   # Repositorios y use-package
│   │   ├── core-defaults.el   # Encoding, backups, defaults
│   │   ├── core-ui.el         # Tema, modeline, iconos
│   │   └── core-keybindings.el
│   ├── completion/
│   │   ├── completion-vertico.el   # Minibuffer vertical
│   │   ├── completion-consult.el   # Búsqueda y navegación
│   │   └── completion-corfu.el     # Autocompletado in-buffer
│   ├── editor/
│   │   ├── editor-defaults.el      # Multiple cursors, parens
│   │   └── editor-snippets.el      # Yasnippet
│   ├── tools/
│   │   ├── tools-magit.el          # Git
│   │   ├── tools-project.el        # Proyectos
│   │   └── tools-treemacs.el       # File tree
│   └── lang/
│       ├── lang-lsp.el             # LSP base
│       ├── lang-python.el          # Python + pyright + ruff
│       ├── lang-typescript.el      # TypeScript/React + nvm.el
│       └── lang-markdown.el        # Markdown + mermaid
│   └── tools/
│       └── tools-ai.el             # AI: gptel + GitHub Copilot
└── tree-sitter/
    ├── libtree-sitter-python.so
    ├── libtree-sitter-typescript.so
    ├── libtree-sitter-tsx.so
    ├── libtree-sitter-javascript.so
    └── libtree-sitter-json.so
```

## Decisiones de Diseño

| Antes | Ahora | Razón |
|-------|-------|-------|
| Helm | Vertico + Consult | Más modular, mejor rendimiento |
| Company | Corfu + Cape | Más simple, mejor integración |
| Projectile | project.el | Built-in en Emacs 28+ |
| undo-tree | undo-redo | Built-in en Emacs 28+ |

### Python

- **pyright** para tipos y completions (instalado globalmente)
- **ruff** para linting/formatting (preferencia por versión del proyecto)
- **pet** detecta automáticamente virtualenvs de uv, poetry, pipenv

## Keybindings

### Core

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c <arrows>` | windmove-* | Navegar entre ventanas |
| `C-z` | undo | Deshacer |
| `C-S-z` | undo-redo | Rehacer |
| `C-x k` | kill-current-buffer | Cerrar buffer actual |
| `C-+` / `C--` | text-scale-adjust | Zoom in/out |
| `C-0` | text-scale-adjust | Reset zoom |

### Dashboard (pantalla de inicio)

| Keybinding | Descripción |
|------------|-------------|
| `r` | Archivos recientes |
| `p` | Proyectos |
| `m` | Bookmarks |
| `g` | Refrescar |

> El dashboard aparece al iniciar Emacs y muestra proyectos recientes, archivos y bookmarks.

### Completion (Vertico/Consult)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-x b` | consult-buffer | Cambiar buffer |
| `C-s` | consult-line | Buscar en buffer |
| `C-r` | consult-line | Buscar hacia atrás |
| `M-s r` | consult-ripgrep | Buscar en proyecto (ripgrep) |
| `M-s g` | consult-grep | Buscar con grep |
| `M-g g` | consult-goto-line | Ir a línea |
| `M-g i` | consult-imenu | Navegar símbolos |
| `C-x f` | consult-recent-file | Archivos recientes |
| `M-y` | consult-yank-pop | Historial de clipboard |
| `C-.` | embark-act | Acciones contextuales |
| `C-;` | embark-dwim | Acción por defecto |

### Editor

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C->` | mc/mark-next-like-this | Marcar siguiente ocurrencia |
| `C-<` | mc/mark-previous-like-this | Marcar anterior ocurrencia |
| `C-c C->` | mc/mark-all-like-this | Marcar todas las ocurrencias |
| `C-S-c C-S-c` | mc/edit-lines | Cursor en cada línea |
| `M-S-<up>` | move-text-up | Mover línea arriba |
| `M-S-<down>` | move-text-down | Mover línea abajo |
| `C-=` | er/expand-region | Expandir selección |
| `C-c d` | zeta/duplicate-line-or-region | Duplicar línea |
| `C-c y` | consult-yasnippet | Buscar snippets |

### LSP

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c l r` | lsp-rename | Renombrar símbolo |
| `C-c l a` | lsp-execute-code-action | Acciones de código |
| `C-c l f` | lsp-format-buffer | Formatear buffer |
| `C-c l d` | lsp-describe-thing-at-point | Documentación |
| `C-c l i` | lsp-find-implementation | Ir a implementación |
| `C-c l t` | lsp-find-type-definition | Ir a definición de tipo |
| `C-c l R` | lsp-find-references | Buscar referencias |
| `C-c l p` | lsp-ui-peek-find-references | Peek referencias |
| `C-c l D` | lsp-ui-doc-glance | Vista rápida de docs |
| `C-c l s` | consult-lsp-symbols | Buscar símbolos |
| `C-c l e` | consult-lsp-diagnostics | Ver diagnósticos |

### Git (Magit)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-x g s` | magit-status | Estado del repo |
| `C-x g c` | magit-commit | Commit |
| `C-x g p` | magit-push | Push |
| `C-x g u` | magit-pull | Pull |
| `C-x g l` | magit-log | Log |
| `C-x g b` | magit-blame | Blame |
| `C-x g d` | magit-diff | Diff |

### Project (project.el + fzf)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-x p p` | project-switch-project | Cambiar proyecto |
| `C-x p f` | zeta/fzf-project-find-file | Buscar archivo (fzf + fdfind) |
| `C-x p g` | zeta/fzf-project-grep | Buscar texto live (fzf + rg) |
| `C-x p b` | project-switch-to-buffer | Cambiar buffer |
| `C-x p d` | project-find-dir | Buscar directorio |
| `C-x p s` | project-shell | Shell en proyecto |
| `C-x p e` | project-eshell | Eshell en proyecto |
| `C-x p c` | project-compile | Compilar |
| `C-x p k` | project-kill-buffers | Cerrar buffers |

> `C-x p f` usa `fdfind` que respeta `.gitignore` automáticamente.
> `C-x p g` usa ripgrep con búsqueda live (resultados mientras escribes).

### fzf (global)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c z` | zeta/fzf-find-file | Buscar archivo (fdfind) |
| `C-c Z` | zeta/fzf-rg-live | Buscar texto live (rg) |
| `M-s .` | zeta/search-selection-in-project | Buscar selección en proyecto |

#### Dentro de fzf (controles de preview)

| Keybinding | Descripción |
|------------|-------------|
| `Ctrl-/` | Toggle preview (bat) |
| `Ctrl-Up` | Scroll preview arriba |
| `Ctrl-Down` | Scroll preview abajo |
| `C-y` | Pegar texto del clipboard |

### Treemacs

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `M-0` | treemacs-select-window | Seleccionar treemacs |
| `C-x t t` | treemacs | Toggle treemacs |
| `C-x t 1` | treemacs-delete-other-windows | Solo treemacs |
| `C-x t d` | treemacs-select-directory | Seleccionar directorio |
| `C-x t C-f` | treemacs-find-file | Buscar archivo |

> Treemacs se sincroniza automáticamente con el proyecto actual al abrirse o al cambiar de proyecto.

### Python

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-f` | zeta/python-format-buffer | Formatear con ruff |
| `C-c C-p` | run-python | Iniciar intérprete |

### TypeScript/React

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-f` | zeta/prettier-format-buffer | Formatear con Prettier |
| `C-c C-e` | zeta/eslint-fix-buffer | Arreglar errores ESLint |
| `C-c l r` | lsp-rename | Renombrar símbolo |
| `C-c l a` | lsp-execute-code-action | Quick fix, imports |
| `C-c l R` | lsp-find-references | Buscar referencias |

> Los keybindings LSP (`C-c l ...`) son los mismos que para Python.
> Prettier y ESLint usan automáticamente los binarios del proyecto (`node_modules/.bin/`).

### Markdown

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-p` | markdown-preview | Preview en navegador |
| `C-c C-e` | markdown-export | Exportar |
| `C-c C-g` | grip-mode | Preview estilo GitHub |
| `C-c C-m` | zeta/markdown-preview-with-mermaid | Preview con mermaid renderizado |
| `C-c C-l` | markdown-insert-link | Insertar link |
| `C-c C-i` | markdown-insert-image | Insertar imagen |
| `C-c C-t` | markdown-toc-generate-or-refresh-toc | Generar TOC |
| `C-c '` | markdown-edit-code-block | Editar bloque de código |

> **Nota**: `C-c C-m` genera un HTML temporal con mermaid.js y lo abre en el navegador. Los diagramas mermaid se renderizan automáticamente.

### Mermaid (archivos .mmd)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-c` | mermaid-compile | Compilar archivo a imagen |
| `C-c C-b` | mermaid-compile-buffer | Compilar buffer |
| `C-c C-r` | mermaid-compile-region | Compilar región |
| `C-c C-o` | mermaid-open-browser | Abrir editor live online |
| `C-c C-d` | mermaid-open-doc | Abrir documentación |

### AI (gptel + Copilot)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c a a` | gptel | Abrir/toggle buffer de chat |
| `C-c a s` | gptel-send | Enviar región/buffer al LLM |
| `C-c a m` | gptel-menu | Menú transient con opciones |
| `C-c a r` | gptel-rewrite | Reescribir región con IA |
| `C-c a b` | gptel-add | Agregar archivo/contexto al chat |
| `C-c a k` | gptel-abort | Cancelar request en progreso |
| `C-c a e` | zeta/ai-explain-region | Explicar código seleccionado |
| `C-c a i` | zeta/ai-improve-region | Sugerir mejoras al código |
| `C-c a d` | zeta/ai-generate-docstring | Generar docstring |
| `Tab` | copilot-accept-completion | Aceptar sugerencia de Copilot |
| `C-Tab` | copilot-accept-completion-by-word | Aceptar palabra por palabra |
| `M-n` / `M-p` | copilot-next/previous-completion | Navegar sugerencias |

> `C-c a m` abre un menú transient donde puedes cambiar modelo, backend (Claude/Gemini), system prompt, etc.

## Notas

### Per-project Python/Ruff

La configuración usa `pet` para detectar el virtualenv de cada proyecto. Esto significa que:

1. Python y ruff se toman del venv del proyecto si existen
2. Si no hay ruff en el proyecto, usa el global instalado con `uv tool`
3. pyright siempre es global pero analiza usando el Python del proyecto

Para un proyecto nuevo:
```bash
uv init my-project
cd my-project
uv add ruff --dev
```

### Tree-sitter

Tree-sitter mejora el syntax highlighting usando un parser real en lugar de regex. La versión del grammar debe coincidir con la ABI que soporta Emacs:

- Emacs 30.2 soporta ABI 13-14
- tree-sitter-python v0.23.2 usa ABI 14
- Versiones más nuevas usan ABI 15 (incompatible)

### Auto-discovery de proyectos

Los proyectos en `~/dev` se descubren automáticamente al iniciar Emacs. Para agregar más directorios, editar `zeta/project-dirs` en `tools-project.el`.
