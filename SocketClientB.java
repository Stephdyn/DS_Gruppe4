
import java.io.*;
import java.net.*;

// Unterschied zu 1a) keine Sockets, kein manuelles Anlegen von HTTP-Header und Verbindung
public class SocketClientB {

    public static void main(String[] args) throws URISyntaxException {

        //System.setProperty("http.proxyHost", "https://www.google.de");
        //System.setProperty("http.proxyPort", "80");

        try {
            URL url = URI.create("https://www.fh-wedel.de").toURL();

            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");

            InputStreamReader inputStream = new InputStreamReader(connection.getInputStream());
            BufferedReader in = new BufferedReader(inputStream);
            
            String responseLine;
            while ((responseLine = in.readLine()) != null) {
                System.out.println(responseLine);
            }

        } catch (IOException e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
