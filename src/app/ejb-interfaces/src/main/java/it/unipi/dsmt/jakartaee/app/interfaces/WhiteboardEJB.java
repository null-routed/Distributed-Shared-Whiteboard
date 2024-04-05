package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import jakarta.ejb.Remote;
import jakarta.validation.constraints.NotNull;


import java.util.List;

@Remote
public interface WhiteboardEJB {
    List<MinimalWhiteboardDTO> searchWhiteboard(String name);
    List<MinimalWhiteboardDTO> getAllWhiteboards(String userID);
    List<MinimalWhiteboardDTO> getSharedWhiteboards(String ID);
    MinimalWhiteboardDTO getWhiteboardByID(int whiteboardID);
    boolean addWhiteboard(@NotNull String ownerID, WhiteboardCreationDTO newWhiteboard);
    boolean removeSharedWhiteboard(@NotNull String username, int whiteboardID);
    boolean deleteWhiteboard(int ID);
    List<String> getParticipantUsernames(int whiteboardID);
}
