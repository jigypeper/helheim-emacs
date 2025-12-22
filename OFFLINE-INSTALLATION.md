# Offline Installation Guide

This guide explains how to create and use an offline bundle of Helheim Emacs for Linux machines without internet access.

## Part 1: Creating the Bundle (on machine WITH internet)

1. **First-time setup:**
   ```bash
   cd helheim-emacs
   cp init.example.el init.el
   emacs --init-directory .
   ```
   Let Emacs start and download all packages (this may take a few minutes).
   Once complete, quit Emacs.

2. **Create the offline bundle:**
   ```bash
   ./create-offline-bundle.sh
   ```
   This creates a file like `helheim-emacs-offline-20251221.tar.gz` containing everything.

3. **Transfer to offline machine:**
   - Copy the `.tar.gz` file to the target Linux machine using USB drive, network transfer, etc.

## Part 2: Installation (on offline Linux machine)

### Prerequisites

The target machine needs these system packages installed:
- **Emacs 29.1 or later**
- **git** (required even offline - used internally by some packages)
- **ripgrep** (`rg`) - for deadgrep functionality

Install on Debian/Ubuntu:
```bash
sudo apt install emacs git ripgrep
```

Install on RHEL/Fedora:
```bash
sudo dnf install emacs git ripgrep
```

Install on Arch:
```bash
sudo pacman -S emacs git ripgrep
```

### Fonts

You need two fonts installed:

1. **Symbols Nerd Font** (required for icons)
   - Download from: https://github.com/ryanoasis/nerd-fonts/releases
   - Look for `SymbolsNerdFont.zip` or `SymbolsNerdFontMono.zip`
   - Extract and install the `.ttf` or `.otf` files

2. **Cascadia Code** (default text font, or use your preferred font)
   - Download from: https://github.com/microsoft/cascadia-code/releases
   - Install the `.ttf` files

To install fonts on Linux:
```bash
# System-wide installation
sudo cp *.ttf /usr/share/fonts/truetype/
sudo fc-cache -f -v

# Or user installation
mkdir -p ~/.local/share/fonts
cp *.ttf ~/.local/share/fonts/
fc-cache -f -v
```

### Language Servers (Optional)

Helheim now includes Eglot LSP support. For full IDE features (go-to-definition, auto-completion, etc.), install language servers for your programming languages:

**C/C++:**
```bash
# Debian/Ubuntu
sudo apt install clangd

# RHEL/Fedora
sudo dnf install clang-tools-extra

# Arch
sudo pacman -S clang
```

**Python:**
```bash
pip install pyright
# or
pip install python-lsp-server
```

**Rust:**
```bash
rustup component add rust-analyzer
```

**Go:**
```bash
go install golang.org/x/tools/gopls@latest
```

**JavaScript/TypeScript:**
```bash
npm install -g typescript-language-server typescript
```

Without language servers, Eglot will be disabled and you'll fall back to dumb-jump (regex-based go-to-definition).

### Setup

1. **Extract the bundle:**
   ```bash
   tar -xzf helheim-emacs-offline-YYYYMMDD.tar.gz
   cd helheim-emacs
   ```

2. **Configure init.el:**
   If not already done, rename the example init file:
   ```bash
   cp init.example.el init.el
   ```
   
   Edit `init.el` to change the font if needed (line 11):
   ```elisp
   (let ((font (font-spec :family "Your Font Name" :size 13.0 :weight 'normal)))
     ...)
   ```

3. **Run Emacs:**
   ```bash
   emacs --init-directory ./helheim-emacs
   ```

## Notes

- **Architecture compatibility:** If the offline machine has a different CPU architecture than where you created the bundle, Emacs may need to recompile some native extensions on first run (this happens automatically but requires a compiler).

- **No updates:** Without internet, you won't be able to update packages. To update, create a new bundle from a machine with internet access.

- **Portability:** You can move the entire `helheim-emacs` directory anywhere. Use `emacs --init-directory <path>` to launch with that configuration.

- **System-wide installation:** To make it the default Emacs config:
  ```bash
  mv helheim-emacs ~/.config/emacs
  # Then just run: emacs
  ```

## Troubleshooting

**Problem:** Native compilation errors on first run  
**Solution:** Install GCC and required build tools:
```bash
# Debian/Ubuntu
sudo apt install build-essential

# RHEL/Fedora
sudo dnf groupinstall "Development Tools"
```

**Problem:** Icons don't display  
**Solution:** Verify Symbols Nerd Font is installed:
```bash
fc-list | grep -i "nerd\|symbol"
```

**Problem:** "Cannot find git executable"  
**Solution:** Install git even if you don't need version control - some packages use it internally.
