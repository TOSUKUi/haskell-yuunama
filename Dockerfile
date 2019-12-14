FROM fpco/stack-build

RUN sed -i 's@archive.ubuntu.com@ftp.jaist.ac.jp/pub/Linux@g' /etc/apt/sources.list && \
    apt update && apt install -y libgtk2.0-dev libglib2.0-dev libsdl2-dev

