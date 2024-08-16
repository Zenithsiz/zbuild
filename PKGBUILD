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
sha512sums=('1fb627ed58f6f09412bc0d03b63e8c2d04b202253fafe409443a8d1a2a24d797099d2289146306d8b4195ae1df25286be23b96f0610e152cd5a2b6b4ddbfa818')

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
