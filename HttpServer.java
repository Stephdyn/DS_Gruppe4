import java.io.*;
import java.net.*;

public class HttpServer {
    public static void main(String[] args) throws IOException {
        int port = 8080;  // Port number where the server will listen for requests
        ServerSocket serverSocket = new ServerSocket(port);
        System.out.println("Server listening on port " + port);

        // Main server loop
        while (true) {
            Socket clientSocket = serverSocket.accept();  // Accept incoming connection
            new Thread(new ClientHandler(clientSocket)).start();  // Create new thread for each client
        }
    }
}

class ClientHandler implements Runnable {
    private Socket clientSocket;

    public ClientHandler(Socket socket) {
        this.clientSocket = socket;
    }

    @Override
    public void run() {
        try (BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
             PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true)) {
            
            
            String requestLine = in.readLine();
            System.out.println("Received request: " + requestLine);

            if (requestLine != null && requestLine.startsWith("GET")) {
                String[] tokens = requestLine.split(" ");
                String filePath = tokens[1].equals("/") ? "index.html" : tokens[1].substring(1);
                System.out.println(filePath);

                File file = new File("./index.html");
                System.out.println(file);
                //if (file.exists()) {
                    // Send HTTP headers
                    out.println("HTTP/1.1 200 OK");
                    out.println("Content-Type: text/html");
                    out.println("Content-Length: " + file.length());
                    out.println();

                    // Send file content
                    BufferedReader fileReader = new BufferedReader(new FileReader(file));
                    String line;
                    while ((line = fileReader.readLine()) != null) {
                        out.println(line);
                    }
                    fileReader.close();
                /* } else {
                    // Send a 404 Not Found response if the file does not exist
                    out.println("HTTP/1.1 404 Not Found");
                    out.println();
                    out.println("File Not Found");
                } */
            }

            // Close client socket after handling the request
            clientSocket.close();
        } catch (IOException e) {
            System.out.println("Error handling client request: " + e.getMessage());
        }
    }
}
