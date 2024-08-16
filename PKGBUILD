# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=zbuild
pkgver=0.1.8
pkgrel=1
pkgdesc="A make-like generic build system "
arch=('x86_64')
url="https://github.com/zenithsiz/zbuild"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('d01178dd9a7ba82c844db0f18dd6474d5a658655051be3cd7b77ef714800bff79c1daf5775cb14b7611797285274fa50def8dd1f67d7e2c98a5ae60318c0b864')

prepare() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo fetch --locked --target "$(rustc -vV | sed -n 's/host: //p')"
}

build() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo build --frozen --release --all-features
}

check() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo test --frozen --all-features
}

package() {
	cd "$pkgname-$pkgver"

	install -Dm0755 -t "$pkgdir/usr/bin/" "target/release/$pkgname"

	install -Dm0644 -t "$pkgdir/usr/share/doc/${pkgname}" "README.md"
	install -Dm0644 -t "$pkgdir/usr/share/licenses/${pkgname}" "LICENSE-MIT"
	install -Dm0644 -t "$pkgdir/usr/share/licenses/${pkgname}" "LICENSE-APACHE"
}
