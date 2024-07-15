# .emacs.x

An Emacs configuration that meets my minimum requirements. 

# 安装（window）

直接到 [GNU Emacs](https://www.gnu.org/software/emacs/download.html) 下载 window 安装包。

## 设置 HOME 环境变量

Emacs 会优先到 HOME 环境变量指定的地址去寻找配置文件。

## 安装 msys2 和 unix tools

```
  # msys2
  winget install -e --id MSYS2.MSYS2
  pacman -Syu
  # diff
  pacman -S diffutils
  # aspell
  pacman -S ucrt64/mingw-w64-ucrt-x86_64-aspell
  pacman -S ucrt64/mingw-w64-ucrt-x86_64-aspell-en
```

设置 msys2 环境。新增 MSYSTEM 环境变量，值为 UCRT64。

## 安装 gnupg

```
  winget install -e --id GnuPG.GnuPG
```

导入私钥：

```
  gpg --import sub_private_key.asc
```

配置 gnupg，在 %APPDATA%\gnupg 目录下新建 gpg-agent.conf 文件，并写入下面两条配置：

```
  allow-emacs-pinentry
  allow-loopback-pinentry
```

## 安装 rg/find/grep/git

```
  winget install BurntSushi.ripgrep.GNU
  winget install GnuWin32.FindUtils
  winget install GnuWin32.Grep
  winget install --id Git.Git
  winget install -e --id sharkdp.fd
```

# 参考

1. [emacs-bedrock](https://sr.ht/~ashton314/emacs-bedrock/)
