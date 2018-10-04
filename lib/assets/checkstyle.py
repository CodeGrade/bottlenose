from pycodestyle import BaseReport, StyleGuide, get_parser
import json
import os

def json_load_byteified(file_handle):
    return _byteify(
        json.load(file_handle, object_hook=_byteify),
        ignore_dicts=True
    )

def json_loads_byteified(json_text):
    return _byteify(
        json.loads(json_text, object_hook=_byteify),
        ignore_dicts=True
    )

def _byteify(data, ignore_dicts = False):
    # if this is a unicode string, return its string representation
    if isinstance(data, unicode):
        return data.encode('utf-8')
    # if this is a list of values, return list of byteified values
    if isinstance(data, list):
        return [ _byteify(item, ignore_dicts=True) for item in data ]
    # if this is a dictionary, return dictionary of byteified keys and values
    # but only if we haven't already byteified it
    if isinstance(data, dict) and not ignore_dicts:
        return {
            _byteify(key, ignore_dicts=True): _byteify(value, ignore_dicts=True)
            for key, value in data.iteritems()
        }
    # if it's anything else, return it in its original form
    return data

class TapErrorReport(BaseReport):
    """Print the results of checks in TAP format"""

    def __init__(self, options):
        options.select = []
        self._errors = []
        self.config = {}
        for conf_file in options.grade_config:
            self.merge(self.config, json_load_byteified(open(conf_file)))
        self.preprocess_config()
        self.deductions = {}
        self.max_points = options.max_points
        super(TapErrorReport, self).__init__(options)

    def merge(self, a, b):
        "merges b into a"
        for key in b:
            if key in a:
                if isinstance(a[key], dict) and isinstance(b[key], dict):
                    self.merge(a[key], b[key])
                else:
                    a[key] = b[key]
            else:
                a[key] = b[key]

    def preprocess_chain(self, code):
        if (len(code) <= 1):
            return self.config.get(code)
        prefix = code[:len(code) - 1]
        parent = self.config.get(prefix, self.preprocess_chain(prefix))
        cur_config = self.config.get(code, dict())
        cur_config["code"] = code
        cur_config["description"] = cur_config.get("description", parent.get("description"))
        cur_config["deduction"] = cur_config.get("deduction", parent.get("deduction"))
        cur_config["severity"] = cur_config.get("severity", parent.get("severity")).lower()
        cur_config["maximumDeductions"] = cur_config.get("maximumDeductions", parent.get("maximumDeductions"))
        return cur_config
        
    def preprocess_config(self):
        for key, value in self.config.iteritems():
            if isinstance(value, dict):
                self.preprocess_chain(key)

    def lookup(self, code):
        if (code == ""): return None
        cur = self.config.get(code)
        if (cur):
            return cur
        else:
            return self.lookup(code[:len(code) - 1])

    def deduction(self, filename, code):
        if self.suppressed(filename, code):
            return 0
        else:
            return self.lookup(code).get("deduction")

    def suppressed(self, filename, code):
        if self.deductions[filename]["total"] > self.config.get("maximum deductions per file"):
            return True
        if self.deductions[filename][code] > self.lookup(code).get("maximumDeductions"):
            return True
        return False

    def ignored(self, filename, code):
        self.lookup(code).get("severity") == "ignore"
                
    def error(self, line_number, offset, text, check):
        code = text[:4]
        if not self.ignored(self.filename, code):
            text = "%s.  Problem occurs at column %d" % (text[5:], offset)
            file_deductions = self.deductions[self.filename] = self.deductions.get(self.filename, {})
            file_deductions["total"] = file_deductions.get("total", 0) + 1
            file_deductions[code] = file_deductions.get(code, 0) + 1
            self._errors.append([text, self.filename, line_number, offset, code,
                                 self.deduction(self.filename, code), self.suppressed(self.filename, code)])
        return super(TapErrorReport, self).error(line_number, offset, text, check)

    def print_results(self):
        print("TAP version 13")
        print("1..%d" % self.total_errors)
        print("# Time: %f" % self.elapsed)
        print("# TOTAL POINTS: %f" % self.max_points)
        print("# Tests run: %d" % len(self._errors))
        for i, err in enumerate(self._errors, start=1):
            print("not ok %d %s" % (i, err[4]))
            print("# More information")
            print("  ---")
            print("  message: \"%s\"" % err[0].replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "\\\""))
            print("  filename: \"%s\"" % err[1])
            print("  line: %d" % err[2])
            print("  category: \"%s\"" % self.lookup(err[4])["description"])
            print("  severity: %s" % self.lookup(err[4])["severity"].title())
            print("  weight: %d" % err[5])
            print("  suppressed: %s" % ("true" if err[6] else "false"))
            print("  ...")


def _main():
    """Parse options and run checks on Python source."""
    import signal

    # Handle "Broken pipe" gracefully
    try:
        signal.signal(signal.SIGPIPE, lambda signum, frame: sys.exit(1))
    except AttributeError:
        pass    # not supported on Windows

    parser = get_parser()
    parser.add_option("--grade-config", action="append", dest="grade_config")
    parser.add_option("--max-points", type="float", dest="max_points")
    style_guide = StyleGuide(parser=parser, parse_argv=True)
    options = style_guide.options
    style_guide.init_report(TapErrorReport)

    report = style_guide.check_files()

    report.print_results()


if __name__ == '__main__':
    _main()
