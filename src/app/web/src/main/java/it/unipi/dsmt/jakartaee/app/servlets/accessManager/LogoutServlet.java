package it.unipi.dsmt.jakartaee.app.servlets.accessManager;

import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;


/**
 * Servlet to handle logout request.
 */
@WebServlet(name = "LogoutServlet", value = "/logout")
public class LogoutServlet extends HttpServlet {

    /**
     * Finalizes the logout process by invalidating the session and redirecting to the login page.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if an I/O error occurs during redirection
     */
    private void finalizeLogout(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@LogoutServlet: called finalizeLogout() method");
        request.getSession().invalidate();      // Invalidate the session
        ClientRedirector.redirectToLogin(request, response);        // Redirect to login page
    }

    /**
     * Handles the HTTP GET method by finalizing the logout process.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if an I/O error occurs during the logout process
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

    /**
     * Handles the HTTP POST method by finalizing the logout process.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if an I/O error occurs during the logout process
     */
    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

}