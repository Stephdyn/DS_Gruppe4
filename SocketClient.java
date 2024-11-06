
import java.io.*;
import java.net.*;

public class SocketClient {

    public static void main(String[] args) {
        String host = "localhost";
        int port = 8080;

        try (
            Socket socket = new Socket(host, port); //Verbindung zu FH-Wedel.de anlegen

            //Konsolenausgabe
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true); 

            //BR: Liest Text aus einem Zeicheneingabestream und puffert die gelesenen Zeichen
            //ISR: Konvertierung von Bytes zu Zeichen
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            // HTTP GET request senden
            out.println("GET /index.html HTTP/1.1");
            out.println("Host: " + host);
            out.println("Connection: close");
            out.println();

            // Antwort ausgeben
            String responseLine;
            while ((responseLine = in.readLine()) != null) {
                System.out.println(responseLine);
            }
        } catch (IOException e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
