package it.unipi.dsmt.jakartaee.app.utility;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.List;

/**
 * Static class providing methods for controlling user permissions to webapp resources
 * and managing the storage and retrieval in the HTTP session
 * of information about currently logged user.
 */
public class AccessController {

    private static final String LOGGED_USER_ATTRIBUTE = "logged_user";

    /**
     * Checks if the logged user is allowed to access the requested resource.
     * It redirects the user to its main page if the user is not allowed or
     * to the login page if they are not logged.
     * In these cases, the method returns null.
     *
     * @param request HttpServletRequest object
     * @param response HttpServletResponse object
     * @param requiredRole required role to access the requested resource
     * @return stored instance of LoggedUserDTO if the user is logged AND
     * they're allowed to access the requested resource, null otherwise
     * @throws IOException if redirection fails
     */
/*
    public static @Nullable LoggedUserDTO checkAccess (
            @NotNull HttpServletRequest request,
            @NotNull HttpServletResponse response,
            @NotNull UserRole requiredRole) throws IOException
    {
        // Check is arguments are valid
        if (requiredRole == UserRole.invalid) {
            throw new IllegalArgumentException("Invalid required role");
        }

        // Get currently logged user's information
        LoggedUserDTO loggedUser = getLoggedUserWithRedirect(request, response);
        if (loggedUser == null) {
            return null;
        }

        // Check if current user role against required role to access the resource
        if (loggedUser.getRole() != requiredRole) {
            // User not allowed to access
            ClientRedirector.redirectToPortalPage(request, response, loggedUser.getRole());
            return null;
        }

        return loggedUser;
    }
*/

    /**
     * Get a LoggedUserDTO object corresponding to the currently logged user.
     * If the current user is not logged, it will be redirected to the login page.
     * @param request HttpServletRequest object
     * @param response HttpServletResponse object
     * @return stored instance of LoggedUserDTO if the user is logged, null otherwise
     * @throws IOException if redirection fails
     */
    public static @Nullable LoggedUserDTO getLoggedUserWithRedirect (
            @NotNull HttpServletRequest request,
            @NotNull HttpServletResponse response) throws IOException
    {
        LoggedUserDTO loggedUser = getLoggedUser(request);
        if (loggedUser == null) {
            // User not logged => redirect to the login page
            ClientRedirector.redirectToLogin(request, response);
        }

        return loggedUser;
    }

    /**
     * Get a LoggedUserDTO object corresponding to the currently logged user.
     * @param request HttpServletRequest object
     * @return stored instance of LoggedUserDTO if the user is logged, null otherwise
     */
    public static @Nullable LoggedUserDTO getLoggedUser (@NotNull HttpServletRequest request) {
        return (LoggedUserDTO) request.getSession().getAttribute(LOGGED_USER_ATTRIBUTE);
    }

    /**
     * Set the stored instance of LoggedUserDTO object.
     * @param request HttpServletRequest object
     * @param loggedUser instance of LoggedUserDTO representing currently logged user
     */
    public static void setLoggedUser (@NotNull HttpServletRequest request, @NotNull LoggedUserDTO loggedUser) {
        request.getSession().setAttribute(LOGGED_USER_ATTRIBUTE, loggedUser);
    }

}
