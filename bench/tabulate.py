# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
from typing import Dict
from enum import Enum
import re

class Alignment(Enum):
    LEFT = 0
    RIGHT = 1
    CENTER = 2

class TablePrinter(object):
    def __init__(self, columns):
        assert(len(columns) > 0)
        self._columns = columns
        self._widths = [len(col['label']) for col in self._columns]
        self._rows = []
        pass

    def _convert_field_dict_to_ordered_list(self, fields:Dict[str, object]):
        assert(len(fields) == len(self._columns))

        ordered_list = [None] * len(self._columns)
        column_names = [column['label'] for column in self._columns]

        for column, value in fields.items():
            index = column_names.index(column)
            ordered_list[index] = value
        return ordered_list

    def _print_row(self, row, align_style=None):
        for i, (value, column, align_width) in enumerate(zip(row, self._columns, self._widths)):
            if i > 0:
                print(' | ', end='')

            actual_align_style = align_style if align_style != None else column['align']
            align_char = {
                Alignment.LEFT: '<',
                Alignment.CENTER: '^',
                Alignment.RIGHT: '>'
            }[actual_align_style]
            print('{0:{align_style}{align_width}}'.format(value, align_style=align_char, align_width=align_width), end=' ')
        print()
        pass

    def _print_horizontal_separator(self):
        for i, align_width in enumerate(self._widths):
            if i > 0:
                print('-+-', end='')
            print('-' * (align_width+1), end='')
        print()
        pass

    def clean_colorama(self, str):
        return re.compile(r'(?:\x1B[@-_]|[\x80-\x9F])[0-?]*[ -/]*[@-~]').sub('', str)

    def add_row(self, fields:Dict[str, object]):
        fields = self._convert_field_dict_to_ordered_list(fields)

        for i, value in enumerate(fields):
            
            self._widths[i] = max(self._widths[i], len(self.clean_colorama(str(value))))

        self._rows.append(fields)

    def _compute_summary_row(self):
        sums = [0] * len(self._widths)
        for row in self._rows:
            for i, value in enumerate(row):
                if not isinstance(value, int):
                    continue
                sums[i] = sums[i] + value
        sums[0] = "Total"
        return sums

    def print(self, summary=False):
        self._print_row([column['label'] for column in self._columns], align_style=Alignment.LEFT)
        self._print_horizontal_separator()

        if summary:
            summary_row = self._compute_summary_row()
            for i, value in enumerate(summary_row):
                self._widths[i] = max(self._widths[i], len(str(value)))

        for row in self._rows:
            self._print_row(row)

        if summary:
            self._print_horizontal_separator()
            self._print_row(summary_row)

        pass
