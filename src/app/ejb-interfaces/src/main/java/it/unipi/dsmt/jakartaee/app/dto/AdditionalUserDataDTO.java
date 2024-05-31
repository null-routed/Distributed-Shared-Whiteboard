package it.unipi.dsmt.jakartaee.app.dto;

import org.jetbrains.annotations.NotNull;
import java.io.Serializable;


/**
 * DTO to store additional information of a logged user
 * in the application, when requested.
 */
public class AdditionalUserDataDTO implements Serializable {
    private String name;
    private String surname;
    private String email;
    private String username;

    public AdditionalUserDataDTO (@NotNull String name, @NotNull String surname, @NotNull String email, @NotNull String username) {
        this.name = name;
        this.surname = surname;
        this.email = email;
        this.username = username;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }
}
