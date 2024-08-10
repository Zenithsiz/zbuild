# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=zbuild
pkgver=0.1.7
pkgrel=1
pkgdesc="A make-like generic build system "
arch=('x86_64')
url="https://github.com/zenithsiz/zbuild"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('2287b5f23afe56148a82a4149ebec5e9c966eeb5f21fb6211fe3af57e849ae85b7867fb92f3d9d1dff81d7a59313f1a1cfc14b7ce7ade42c1817c65062a5fb49')

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
