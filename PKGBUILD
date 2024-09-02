# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=zbuild
pkgver=0.1.9
pkgrel=1
pkgdesc="A make-like generic build system "
arch=('x86_64')
url="https://github.com/zenithsiz/zbuild"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('682afc2e294fb1179a64e8754e22d98bad0055c3bed037d2ae65f095c285d6e0d5441572f733e241f9c2aa389b6f4b69124a5e89269b1e7fbc4ecdae4643d91a')

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
