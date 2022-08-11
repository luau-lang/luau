# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
import os
import platform
import shlex
import socket
import sys
import requests

_hostname = socket.gethostname()

def tag_value(value):
    value = str(value)
    for escape in [',', '=', ' ']:
        value = value.replace(escape, '\\' + escape)
    return value

def field_value(value):
    # Line protocol requires all strings to be quoted
    if not isinstance(value, str):
        return str(value).lower()

    # String values must always be surrounded in unescaped double quotes, while escaping inner quotes with a
    # backslash.
    return '"' + value.replace('"', '\\"') + '"'

class InfluxReporter:
    def __init__(self, args):
        self.args = args
        self.lines = []

    def _send_line_message(self, tags, fields):
        tags_str = ','.join(sorted(tags))
        fields_str = ','.join(fields)
        line_message = '{},{} {}'.format('robench', tags_str, fields_str)

        self.lines.append(line_message)
        if self.args.print_influx_debugging:
            print('[influx] ' + line_message)

    def flush(self, process_exit_code):
        if self.args.report_metrics:
            print('Reporting results to Influx.')
            request = '\n'.join(self.lines)
            try:
                # We don't want a failure to report metrics to influx to result in a failed test suite.
                # Just log a warning instead.
                response = requests.post(url=self.args.report_metrics, data=request)
            except Exception as e:
                print("Unable to report metrics to influx.  Reason: " + str(e))
                print('Request content (for debugging): ')
                print(request)
                pass

    def report_result(self, testFolder, testName, testPath, status, timeMin, timeAvg, timeMax, confInt, vmName, vmPath):
        is_teamcity = 'TEAMCITY_PROJECT_NAME' in os.environ
        tags = [
            'hostname={}'.format(tag_value(_hostname)),
            'is_teamcity={}'.format(tag_value(is_teamcity)),
            'platform={}'.format(tag_value(sys.platform)),
            'type=event',

            # Necessary in order for ElasticSearch to accept this line
            'priority=high',

            'test_folder={}'.format(tag_value(testFolder)),
            'test_name={}'.format(tag_value(testName)),
            'test_path={}'.format(tag_value(testPath)),

            'vm_name={}'.format(tag_value(vmName)),
            'vm_path={}'.format(tag_value(vmPath))
        ]
        fields = [
            'status={}'.format(field_value(status)),
            'time_min={}'.format(timeMin),
            'time_avg={}'.format(timeAvg),
            'time_max={}'.format(timeMax),
            'time_conf_int={}'.format(confInt)
        ]

        self._send_line_message(tags, fields)
