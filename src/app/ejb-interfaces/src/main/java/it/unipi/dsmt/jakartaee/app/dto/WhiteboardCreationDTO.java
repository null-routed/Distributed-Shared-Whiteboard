package it.unipi.dsmt.jakartaee.app.dto;

import java.io.Serializable;


/**
 * DTO to store the necessary information to create a new whiteboard
 * in the application.
 */
public class WhiteboardCreationDTO implements Serializable {

    private String name;
    private String description;
    private boolean readOnly;

    public WhiteboardCreationDTO(String name, String description, boolean readOnly) {
        this.name = name;
        this.description = description;
        this.readOnly = readOnly;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }
}
