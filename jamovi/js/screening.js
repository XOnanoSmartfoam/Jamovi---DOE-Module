'use strict';

module.exports = {
    view_loaded: function(ui) {
        this.installListKeyboardNavigation(ui, ['factors', 'responses']);
    },

    installListKeyboardNavigation: function(ui, names) {
        for (const name of names) {
            const control = ui[name];
            if (!control)
                continue;

            let root = control.el || control.$el;
            if (root && root.jquery)
                root = root[0];
            else if (root && root[0] instanceof HTMLElement)
                root = root[0];
            if (!(root instanceof HTMLElement))
                continue;

            const removeDeleteButtonsFromTabOrder = () => {
                for (const button of root.querySelectorAll('.list-item-delete-button'))
                    button.tabIndex = -1;
            };

            removeDeleteButtonsFromTabOrder();
            new MutationObserver(removeDeleteButtonsFromTabOrder)
                .observe(root, { childList: true, subtree: true });

            root.addEventListener('keydown', event => {
                if (event.key !== 'Tab' || event.shiftKey)
                    return;

                const cell = event.target.closest('.list-item-cell');
                if (!cell || !cell.data)
                    return;

                const row = cell.data.row;
                const column = cell.data.column;
                const nextCells = Array.from(root.querySelectorAll('.list-item-cell'))
                    .filter(candidate => candidate.data &&
                        candidate.data.row === row &&
                        candidate.data.column > column)
                    .sort((a, b) => a.data.column - b.data.column);

                let next = null;
                for (const candidate of nextCells) {
                    next = candidate.querySelector(
                        'input:not([type="hidden"]), textarea, select, ' +
                        '[contenteditable="true"], [role="textbox"]');
                    if (next)
                        break;
                }

                if (!next)
                    next = root.querySelector('.column-add-button');
                if (!next)
                    return;

                event.preventDefault();
                event.stopPropagation();
                next.focus();
            }, true);
        }
    }
};
