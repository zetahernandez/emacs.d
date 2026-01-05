# Emacs Configuration

Configuración modular para Emacs 30.2+ con stack moderno de completion y LSP.

## Características

- **Arquitectura modular** - Configuración dividida en módulos lógicos
- **Completion moderno** - Vertico + Consult + Corfu (reemplaza Helm + Company)
- **LSP** - lsp-mode con pyright + ruff para Python
- **Git** - Magit + diff-hl
- **Proyectos** - project.el built-in (reemplaza Projectile)
- **File tree** - Treemacs con nerd-icons
- **Tree-sitter** - Syntax highlighting mejorado para Python

## Requisitos

- Ubuntu/Linux
- Emacs 30.2+ compilado desde source
- Node.js (para mermaid-cli)
- uv (para herramientas Python)

## Instalación

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

### 2. Clonar esta configuración

```bash
# Backup de configuración existente (si existe)
mv ~/.emacs.d ~/.emacs.d.backup

# Clonar
git clone <repo-url> ~/.emacs.d
```

### 3. Dependencias para Python

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

### 4. Dependencias para Markdown

```bash
# Pandoc - export y preview
sudo apt install pandoc

# Grip - preview estilo GitHub
uv tool install grip

# Mermaid CLI - diagramas
npm install -g @mermaid-js/mermaid-cli
```

### 5. Tree-sitter grammar para Python

```bash
# IMPORTANTE: Usar versión v0.23.2 (ABI 14)
# Las versiones nuevas usan ABI 15 que Emacs 30.2 no soporta

mkdir -p ~/.emacs.d/tree-sitter
git clone --depth 1 -b v0.23.2 \
  https://github.com/tree-sitter/tree-sitter-python.git /tmp/ts-python
cd /tmp/ts-python
cc -fPIC -shared -o libtree-sitter-python.so \
  src/parser.c src/scanner.c -I src -O2
cp libtree-sitter-python.so ~/.emacs.d/tree-sitter/
rm -rf /tmp/ts-python
```

### 6. Instalar fuentes (dentro de Emacs)

```
M-x nerd-icons-install-fonts RET
M-x all-the-icons-install-fonts RET
```

### 7. Iniciar Emacs

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
│       └── lang-markdown.el        # Markdown + mermaid
└── tree-sitter/
    └── libtree-sitter-python.so    # Grammar compilado
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

### Project (project.el)

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-x p p` | project-switch-project | Cambiar proyecto |
| `C-x p f` | project-find-file | Buscar archivo |
| `C-x p g` | project-find-regexp | Buscar texto |
| `C-x p b` | project-switch-to-buffer | Cambiar buffer |
| `C-x p d` | project-find-dir | Buscar directorio |
| `C-x p s` | project-shell | Shell en proyecto |
| `C-x p e` | project-eshell | Eshell en proyecto |
| `C-x p c` | project-compile | Compilar |
| `C-x p k` | project-kill-buffers | Cerrar buffers |

### Treemacs

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `M-0` | treemacs-select-window | Seleccionar treemacs |
| `C-x t t` | treemacs | Toggle treemacs |
| `C-x t 1` | treemacs-delete-other-windows | Solo treemacs |
| `C-x t d` | treemacs-select-directory | Seleccionar directorio |
| `C-x t C-f` | treemacs-find-file | Buscar archivo |

### Python

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-f` | zeta/python-format-buffer | Formatear con ruff |
| `C-c C-p` | run-python | Iniciar intérprete |

### Markdown

| Keybinding | Comando | Descripción |
|------------|---------|-------------|
| `C-c C-p` | markdown-preview | Preview en navegador |
| `C-c C-e` | markdown-export | Exportar |
| `C-c C-g` | grip-mode | Preview estilo GitHub |
| `C-c C-l` | markdown-insert-link | Insertar link |
| `C-c C-i` | markdown-insert-image | Insertar imagen |
| `C-c C-t` | markdown-toc-generate-or-refresh-toc | Generar TOC |
| `C-c '` | markdown-edit-code-block | Editar bloque de código |

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
