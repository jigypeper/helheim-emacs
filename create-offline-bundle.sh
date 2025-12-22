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
echo "5. Run: emacs --init-directory ./helheim-emacs"
echo ""
echo "Note: The bundle includes compiled packages for your current architecture."
echo "If the target machine has different architecture, recompilation may occur on first run."
