FROM ciantic/haskell-ide-engine:ghc8.6.5

RUN sed -i.bak -e "s%http://archive.ubuntu.com/ubuntu/%http://ftp.iij.ad.jp/pub/linux/ubuntu/archive/%g" /etc/apt/sources.list \
    && apt update \
    && apt install -y \
    libgtk2.0-dev \
    libglib2.0-dev \
    libsdl2-dev
