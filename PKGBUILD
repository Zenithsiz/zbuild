# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=zbuild
pkgver=0.1.10
pkgrel=1
pkgdesc="A make-like generic build system "
arch=('x86_64')
url="https://github.com/zenithsiz/zbuild"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('f5a94c82b8298e40a33598f3d3070cd55d18879733f415326950281aa484ebce4d161f682b2a62ca809629e56b7f681516b3f31bcfc77a9874f9d51fc662e11b')

prepare() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo fetch --locked --target "$(rustc -vV | sed -n 's/host: //p')"
}

build() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo build --frozen --release
}

check() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo test --frozen
}

package() {
	cd "$pkgname-$pkgver"

	install -Dm0755 -t "$pkgdir/usr/bin/" "target/release/$pkgname"

	install -Dm0644 -t "$pkgdir/usr/share/doc/${pkgname}" "README.md"
	install -Dm0644 -t "$pkgdir/usr/share/licenses/${pkgname}" "LICENSE-MIT"
	install -Dm0644 -t "$pkgdir/usr/share/licenses/${pkgname}" "LICENSE-APACHE"
}
