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