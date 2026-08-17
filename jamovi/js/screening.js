'use strict';

module.exports = {
    addToSpreadsheet_changed: function(ui) {
        if (!ui.addToSpreadsheet.value())
            return;
        if (ui.designOutput.value())
            return;
        ui.view.model.options.beginEdit();
        ui.designOutput.setValue(true);
        ui.view.model.options.endEdit();
    }
};
