
pkgname=wmii
pkgver=3.9.2
pkgrel=1
pkgdesc="A lightweight, dynamic window manager for X11"
url="http://wmii.suckless.org"
license=("MIT")
arch=("i686" "x86_64")
depends=("libx11" "libxft" "libxinerama" "libxrandr")
makedepends=("mercurial")
optdepends=("plan9port: for use of the alternative plan9port wmiirc" \
	"python: for use of the alternative Python wmiirc" \
	"ruby-rumai: for use of the alternative Ruby wmiirc")
provides=("wmii")
conflicts=("wmii")
source=()

build()
{
    cd $startdir
    flags=(PREFIX=/usr \
           ETC=/etc    \
           DESTDIR="$pkgdir")

    make "${flags[@]}" || return 1
    make "${flags[@]}" install || return 1

    mkdir -p $pkgdir/etc/X11/sessions
    install -m644 -D ./LICENSE $pkgdir/usr/share/licenses/wmii/LICENSE
}

