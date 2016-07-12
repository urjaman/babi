mkdir -p /opt/src
cd /opt/src
git clone https://github.com/SegFaulty/kilo.git
cd kilo
make
ln -sf /opt/src/kilo/kilo /usr/local/sbin/k
