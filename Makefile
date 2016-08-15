KEYPASS   = $(shell awk -F ' *= *' '/^keypass/{print $$2}' eval.conf)
STOREPASS = $(shell awk -F ' *= *' '/^storepass/{print $$2}' eval.conf)

KEYSTORE = gloseval/src/main/resources/keystore.jks
PEM      = gloseval/src/test/resources/gloseval.pem

all: pem

keystore: $(KEYSTORE)

pem: $(PEM)

$(KEYSTORE):
	keytool -genkey \
		-alias domain \
		-keyalg RSA \
		-validity 365 \
		-keystore $@ \
		-keypass $(KEYPASS) \
		-storepass $(STOREPASS) \
		-dname "CN=localhost, OU=Unknown, O=Unknown, L=Unknown, ST=Unknown, C=Unknown"

$(PEM): $(KEYSTORE)
	keytool -exportcert \
		-rfc \
		-alias domain \
		-keystore $(KEYSTORE) \
		-file $@ \
		-storepass $(STOREPASS) \

clean:
	rm -f $(KEYSTORE)
	rm -f $(PEM)

.PHONY: all clean keystore pem
