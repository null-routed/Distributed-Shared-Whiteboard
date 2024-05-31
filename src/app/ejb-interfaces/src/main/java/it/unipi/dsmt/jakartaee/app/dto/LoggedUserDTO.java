package it.unipi.dsmt.jakartaee.app.dto;

import org.jetbrains.annotations.NotNull;
import java.io.Serializable;


/**
 * DTO to store the necessary information to identify a logged user
 * in the application.
 */
public class LoggedUserDTO implements Serializable {
    private String id;
    private String username;
    
    public LoggedUserDTO (@NotNull String id, @NotNull String username) {
        this.id = id;
        this.username = username;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public String toString() {
        return "LoggedUserDTO{" +
                "id='" + id + '\'' +
                ", username='" + username + '\'' +
                '}';
    }
}
