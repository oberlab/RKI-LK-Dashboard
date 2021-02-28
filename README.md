# RKI-LK-Dashboard

Öffentliche Demo: [https://covid-dashboard.oberlab.de/](https://covid-dashboard.oberlab.de/)

## Lokal bauen und Ausführen mit Docker

Diese Shiny-App kann alternativ auch mittels des beigefügten Dockerfile gebaut und ausgeführt werden.

Dazu muss der Docker Container zuerst gebaut werden mittels folgendem Befehl:

`docker build -t oberlab/rki-dashboard .`

Annahme: Ausführung im selben Verzeichnis wie auch die Datei liegt

So dieser Schritt erfolgreich abgeschlossen wurde, kann die Anwendung dann ausgeführt werden:

`docker run --rm -p 3838:3838 oberlab/rki-dashboard`

Dann kann man die Anwendung lokal aufrufen unter [http://localhost:3838](http://localhost:3838)

## Automatisches Bauen des Docker Images mit GitHub Actions

Im Verzeichnis `.github/workflows` liegt die Datei `build-and-deploy.yml`.

In dieser wird eine Action definiert, die automatisch immer dann läuft, wenn ein Versionstag nach dem Muster `v1.1.1` erstellt und - nach erfolgreichem Build - ans GitHub Repository gepushed wird.

Das entsprechende Package findet sich dann unter: [https://github.com/orgs/oberlab/packages/container/package/rki-lk-dashboard](https://github.com/orgs/oberlab/packages/container/package/rki-lk-dashboard)

## Runterladen und Ausführen des Docker Image aus der GitHub Registry

Das Docker Image kann man sich ganz einfach aus der GitHub Container Registry runterladen und ausführen:

`docker pull ghcr.io/oberlab/rki-lk-dashboard`

Anmerkung: Auf diese Art erhält man immer das aktuellste Image ("latest"). 

Wenn man ein spezifisches Tag haben möchte, muss dieses noch hinten an gestellt werden. Etwa so:

`docker pull ghcr.io/oberlab/rki-lk-dashboard:v0.1.0`

### Ausführen

`docker run --rm -p 3838:3838 ghcr.io/oberlab/rki-lk-dashboard`

Wenn der Container korrekt gestartet ist, kann man so auf die Anwendung zugreifen: [http://localhost:3838](http://localhost:3838)
