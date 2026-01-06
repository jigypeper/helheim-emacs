#!/usr/bin/env bash
# Create offline bundle of Helheim Emacs
# Run this on a machine with internet access after initial setup

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUNDLE_NAME="helheim-emacs-offline-$(date +%Y%m%d).tar.gz"

echo "Creating offline bundle..."
echo "This will bundle the entire Helheim Emacs configuration with all packages."
echo ""

# Check if packages are installed
if [ ! -d "$SCRIPT_DIR/var/elpaca" ]; then
    echo "Error: Elpaca packages not found!"
    echo "Please run Emacs with this configuration first to download all packages."
    echo "Run: emacs --init-directory $SCRIPT_DIR"
    exit 1
fi

# Warn if init.el contains :ensure t which could cause network access
if [ -f "$SCRIPT_DIR/init.el" ] && grep -q ":ensure t" "$SCRIPT_DIR/init.el"; then
    echo "Warning: init.el contains ':ensure t' which may cause Elpaca to attempt"
    echo "downloading packages when offline. This should be removed for offline use."
    echo "Found in:"
    grep -n ":ensure t" "$SCRIPT_DIR/init.el"
    echo ""
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Create temporary directory
TEMP_DIR=$(mktemp -d)
BUNDLE_DIR="$TEMP_DIR/helheim-emacs"

echo "Copying files to temporary location..."
cp -r "$SCRIPT_DIR" "$BUNDLE_DIR"

# Clean up unnecessary files
echo "Cleaning up temporary files..."
cd "$BUNDLE_DIR"
find . -name "*.elc~" -delete
find . -name "*~" -delete
find . -name ".DS_Store" -delete
rm -rf .git

# Remove git repositories and caches to save space
echo "Removing git history and caches..."
find var/elpaca/repos -name ".git" -type d -exec rm -rf {} + 2>/dev/null || true
rm -rf var/elpaca/cache
rm -rf var/eln-cache
rm -rf eln-cache
rm -rf var/autosave
rm -rf auto-save-list

# Remove compiled bytecode to ensure compatibility across Emacs versions
echo "Removing compiled bytecode (for Emacs version compatibility)..."
find var/elpaca/builds -name "*.elc" -type f -delete 2>/dev/null || true
find elpa -name "*.elc" -type f -delete 2>/dev/null || true

# Create tarball
echo "Creating archive..."
cd "$TEMP_DIR"
tar -czf "$SCRIPT_DIR/$BUNDLE_NAME" helheim-emacs/

# Cleanup
rm -rf "$TEMP_DIR"

echo ""
echo "✓ Offline bundle created: $BUNDLE_NAME"
echo ""
echo "To use on offline Linux machine:"
echo "1. Transfer $BUNDLE_NAME to the target machine"
echo "2. Extract: tar -xzf $BUNDLE_NAME"
echo "3. Install system dependencies: emacs (29.1+), git, ripgrep"
echo "4. Install fonts: Symbols Nerd Font, Cascadia Code"
echo "5. (Optional) Install language servers for Eglot LSP:"
echo "   - C/C++: clangd or ccls"
echo "   - Python: pyright or pylsp"
echo "   - Rust: rust-analyzer"
echo "   - Go: gopls"
echo "   - JavaScript/TypeScript: typescript-language-server"
echo "6. Run: emacs --init-directory ./helheim-emacs"
echo ""
echo "Note: The bundle includes compiled packages for your current architecture."
echo "If the target machine has different architecture, recompilation may occur on first run."
