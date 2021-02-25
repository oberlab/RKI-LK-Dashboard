# RKI-LK-Dashboard

## Bauen und Ausführen mit Docker

Diese Shiny-App kann alternativ auch mittels des beigefügten Dockerfile gebaut und ausgeführt werden.

Dazu muss der Docker Container zuerst gebaut werden mittels folgendem Befehl:

`docker build -t oberlab/rki-dashboard .`

Annahme: Ausführung im selben Verzeichnis wie auch die Datei liegt

So dieser  Schritt erfolgreich abgeschlossen wurde kann die Anwendung dann ausgeführt werden:

`docker run --rm -p 3838:3838 oberlab/rki-dashboard`

Dann kann man die Anwendung lokal aufrufen unter

`http://localhost:3838/`

## Automatisches Bauen des Docker Images mit GitHub Actions

Im Verzeichnis `.github/workflows` liegt die Datei `docker-build.yml`.

In dieser wird eine Action definiert, die automatisch immer dann läuft, wenn ein Git Tag erstellt und - nach erfolgreichem Build - ans GitHub Repository gepushed wird.

Das entsprechende Package findet sich dann unter:
`https://github.com/orgs/oberlab/packages`

## Runterladen und Ausführen des Docker Image aus dem GitHub Repository

Das Docker Image kann man sich ganz einfach aus dem GitHub Repository runterladen und ausführen. Allerdings
muss man sich im GitHub Repo immer einloggen, damit es funktioniert, selbst bei Public Images!

## Im GitHub Repository einloggen

Mit diesem Befehl kann man sich im GitHub Repository einloggen:
`docker login docker.pkg.github.com --username <GitHub_Username> --password <personal_access_token>`

Das Personal Access Token kann man sich auf diese Art generieren:
`https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token`

Es ist darauf zu achten, dass das Token das Recht "read:packages" bekommt!

## Das Docker Image vom GitHub Repo runterladen

`docker pull docker.pkg.github.com/oberlab/rki-lk-dashboard/rki-dashboard`

Anmerkung: Auf diese Art erhält man immer das aktuellste Image ("latest"). 

Wenn man ein spezifisches Tag haben möchte, muss dieses noch hinten an gestellt werden. Etwa so:

`docker pull docker.pkg.github.com/oberlab/rki-lk-dashboard/rki-dashboard:0.0.2`

## Das Docker Image ausführen

`docker run --rm -p 3838:3838 oberlab/rki-dashboard`

## Auf die Anwendung zugreifen

Wenn das Docker Image korrekt gestartet ist kann man so auf die Anwendung zugreifen:
`http://localhost:3838/`
