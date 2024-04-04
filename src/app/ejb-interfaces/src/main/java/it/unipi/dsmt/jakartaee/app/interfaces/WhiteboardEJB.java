package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import jakarta.ejb.Remote;
import jakarta.validation.constraints.NotNull;

import java.util.List;

@Remote
public interface WhiteboardEJB {
    List<MinimalWhiteboardDTO> searchDashboards(String name);
    List<MinimalWhiteboardDTO> getAllDashboards(String userId);
    List<MinimalWhiteboardDTO> getSharedDashboards(String id);
    boolean addWhiteboard(@NotNull String ownerId, WhiteboardCreationDTO newWhiteboard);
    boolean removeSharedWhiteboard(@NotNull String username, int whiteboardId);
    boolean deleteWhiteboard(int id);

}
